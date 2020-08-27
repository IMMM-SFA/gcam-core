/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
*
* EXPORT CONTROL
* User agrees that the Software will not be shipped, transferred or
* exported into any country or used in any manner prohibited by the
* United States Export Administration Act or any other applicable
* export laws, restrictions or regulations (collectively the "Export Laws").
* Export of the Software may require some form of license or other
* authority from the U.S. Government, and failure to obtain such
* export control license may result in criminal liability under
* U.S. laws. In addition, if the Software is identified as export controlled
* items under the Export Laws, User represents and warrants that User
* is not a citizen, or otherwise located within, an embargoed nation
* (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
*     and that User is not otherwise prohibited
* under the Export Laws from receiving the Software.
*
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
*
* For further details, see: http://www.globalchange.umd.edu/models/gcam/
*
*/


/*!
* \file capacity_technology.cpp
* \ingroup Objects
* \brief CapacityTechnology class source file.
* \author Pralit Patel, Gokul Iyer
*/

#include "util/base/include/definitions.h"
#include "technologies/include/capacity_technology.h"
#include "containers/include/scenario.h"
#include "util/base/include/xml_helper.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/iinfo.h"
#include "technologies/include/ical_data.h"
#include "technologies/include/iproduction_state.h"
#include "technologies/include/production_state_factory.h"
#include "technologies/include/marginal_profit_calculator.h"
#include "technologies/include/ioutput.h"
#include "technologies/include/generic_output.h"
#include "technologies/include/ishutdown_decider.h"
#include "technologies/include/profit_shutdown_decider.h"
#include "util/base/include/ivisitor.h"
#include "containers/include/market_dependency_finder.h"
#include "sectors/include/sector_utils.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

/*!
* \brief Constructor.
* \param aName Technology name.
* \param aYear Technology year.
*/
CapacityTechnology::CapacityTechnology(const string& aName, const int aYear) :
Technology(aName, aYear),
mMinCapFac( 0.0 ),
mInvestScaleDecider( 0 )
{
}

/*!
* \brief Copy the technology paramaters.
* \details Does not copy variables which should get initialized through normal
*          model operations.
* \param aTech Tech CapacityTechnology to copy.
*/
void CapacityTechnology::copy(const CapacityTechnology& aTech) {
    Technology::copy( aTech );
    mCapacity = aTech.mCapacity;
    mSegCapFac = aTech.mSegCapFac;
    mTrialMarketName = aTech.mTrialMarketName;
    mCapacityMarketName = aTech.mCapacityMarketName;
    mMinCapFac = aTech.mMinCapFac;
}

// ! Destructor
CapacityTechnology::~CapacityTechnology() {
}

//! Parses any input variables specific to derived classes
bool CapacityTechnology::XMLDerivedClassParse(const string& aNodeName, const DOMNode* aCurrNode) {
    bool success = false;
    if( aNodeName == "capacity" ) {
        mCapacity = XMLHelper<double>::getValue( aCurrNode );
        success = true;
    }
    else if( aNodeName == "segment-capacity-factor" ) {
        string segmentName = XMLHelper<string>::getAttr( aCurrNode, "name" );
        double segCapFac = XMLHelper<double>::getValue( aCurrNode );
        mSegCapFac[ segmentName ] = segCapFac;
        success = true;
    }
    else if (aNodeName == "trial-market-name") {
        mTrialMarketName = XMLHelper<string>::getValue(aCurrNode);
        success = true;
    }
    else if (aNodeName == "capacity-market-name") {
        mCapacityMarketName = XMLHelper<string>::getValue(aCurrNode);
        success = true;
    }
    else if( aNodeName == "min-capacity-factor" ) {
        mMinCapFac = XMLHelper<double>::getValue( aCurrNode );
        success = true;
    }
    return success;
}

//! write object to xml output stream
void CapacityTechnology::toDebugXMLDerived(const int aPeriod, ostream& aOut, Tabs* aTabs) const {
    XMLWriteElement(mCapacity, "capacity", aOut, aTabs);
    XMLWriteElement(mTrialMarketName, "trial-market-name", aOut, aTabs);
    XMLWriteElement(mCapacityMarketName, "capacity-market-name", aOut, aTabs);
    XMLWriteElement(mIntermitOutTechRatio, "intermittent-capacity-ratio", aOut, aTabs);
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overridden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const string& CapacityTechnology::getXMLName() const {
    return getXMLNameStatic();
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* The "==" operator that is used when parsing, required this second function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME as a static.
*/
const string& CapacityTechnology::getXMLNameStatic() {
    const static string XML_NAME = "capacity-technology";
    return XML_NAME;
}

//! Clone Function. Returns a deep copy of the current technology.
CapacityTechnology* CapacityTechnology::clone() const {
    CapacityTechnology* clone = new CapacityTechnology( mName, mYear );
    clone->copy( *this );
    return clone;
}

void CapacityTechnology::completeInit(const std::string& aRegionName,
    const std::string& aSectorName,
    const std::string& aSubsectorName,
    const IInfo* aSubsectorInfo,
    ILandAllocator* aLandAllocator)
{
    Technology::completeInit(aRegionName, aSectorName, aSubsectorName, aSubsectorInfo,
        aLandAllocator);
    
    // replace the primary output with a generic output (does not add supply to market)
    delete mOutputs[ 0 ];
    mOutputs[ 0 ] = new GenericOutput( aSectorName );
    // we must call initTechVintageVector again now that we have changed the primary
    // output to ensure it gets its arrays sized correctly
    initTechVintageVector();
    
    // pull out the profit shutdown decider from mShutdownDeciders as we don't want
    // them used in tryDispatch but do want to use it for calcInvestmentCapacityScaleFactor
    for( auto shutdownIter = mShutdownDeciders.begin(); shutdownIter != mShutdownDeciders.end(); ) {
        if( (*shutdownIter)->isSameType( ProfitShutdownDecider::getXMLNameStatic() ) ) {
            if( mInvestScaleDecider ) {
                ILogger& mainLog = ILogger::getLogger("main_log");
                mainLog.setLevel(ILogger::WARNING);
                    mainLog << "Multiple profit shutdown deciders found for " << mName << ", " << mYear
                            << " in region " << aRegionName << " and sector " << aSectorName << endl;
            }
            // move the profit shutdown decider out of the mShutdownDeciders and into mInvestScaleDecider
            delete mInvestScaleDecider;
            mInvestScaleDecider = *shutdownIter;
            shutdownIter = mShutdownDeciders.erase( shutdownIter );
        }
        else {
            ++shutdownIter;
        }
    }
    if( !mInvestScaleDecider ) {
        ILogger& mainLog = ILogger::getLogger("main_log");
        mainLog.setLevel(ILogger::SEVERE);
        mainLog << "No profit shutdown deciders available for capacity investment scale calculation in "
                << mName << ", " << mYear << " in region " << aRegionName << " and sector " << aSectorName << endl;
        abort();
    }

    // Make some tests for bad inputs
    if (mCapacityFactor == 0.0) {
        ILogger& mainLog = ILogger::getLogger("main_log");
        mainLog.setLevel(ILogger::SEVERE);
        mainLog << "Capacity factor not set for technology " << mName << ", " << mYear
                << " in region " << aRegionName << " and sector " << aSectorName << endl;
        abort();
    }

    if (!mTrialMarketName.empty() && mCapacityMarketName.empty()) {
        ILogger& mainLog = ILogger::getLogger("main_log");
        mainLog.setLevel(ILogger::WARNING);
        mainLog << "Capacity market name not read in while trial market name is read in " << mName << ", " << mYear
            << " in region " << aRegionName << " and sector " << aSectorName
            << "Capacity market name will default to region name" << endl;

        mCapacityMarketName = aRegionName;
    }

    if (!mTrialMarketName.empty()) {
        // Create Trial Market if trial-market-name has been read in.
        SectorUtils::createTrialSupplyMarket(aRegionName, mTrialMarketName, mTechnologyInfo.get(), mCapacityMarketName);
        
        // Also create trial market associated with the capacity market name (typically read in as the containing grid region). This
        // will make sure that regions associated with a capacity market would see the same intermittent capacity shares and hence
        // capacity payments. In future, this would also help us ensure that regions associated with a capacity market see the same
        // capacity market price.
        SectorUtils::createTrialSupplyMarket(mCapacityMarketName, mTrialMarketName, mTechnologyInfo.get(), mCapacityMarketName);
        
        // Add to dependency relationships to keep track of.
        MarketDependencyFinder* depFinder = scenario->getMarketplace()->getDependencyFinder();
        depFinder->addDependency(aSectorName, aRegionName,
                                 SectorUtils::getTrialMarketName(mTrialMarketName),
                                 aRegionName);
        
        depFinder->addDependency(aSectorName, mCapacityMarketName,
                                 SectorUtils::getTrialMarketName(mTrialMarketName),
                                 mCapacityMarketName);
    }
}

void CapacityTechnology::initCalc(const string& aRegionName,
    const string& aSectorName,
    const IInfo* aSubsectorInfo,
    const Demographic* aDemographics,
    PreviousPeriodInfo& aPrevPeriodInfo,
    const int aPeriod)
{
    Technology::initCalc(aRegionName, aSectorName, aSubsectorInfo,
        aDemographics, aPrevPeriodInfo, aPeriod);

    if (!mTrialMarketName.empty()) {
        // The renewable trial market is a share calculation so we can give the
        // solver some additional hints that the range should be between 0 and 1.
        SectorUtils::setSupplyBehaviorBounds(SectorUtils::getTrialMarketName(mTrialMarketName),
            aRegionName, 0, 1, aPeriod);
    }


    // We are assuming that a technology that wasn't dispatched at all should get
    // permenently retired in the next period.  This is performed here where we
    // reset the production state with lifetime years of zero if there was no production
    // in the previous year when it could have operated.
    if( aPeriod > scenario->getModeltime()->getFinalCalibrationPeriod() &&
        mProductionState[ aPeriod - 1 ]->isOperating() && ( mOutputs[0]->getPhysicalOutput( aPeriod -1) ) == 0.0 )
    {
        delete mProductionState[ aPeriod];
        mProductionState[ aPeriod] = ProductionStateFactory::create(mYear, 0, mFixedOutput, 0.0, aPeriod).release();
    }
    // In addition we need to check if a technology got prematurely retired last period
    // then it stays retired.
    else if( aPeriod > scenario->getModeltime()->getFinalCalibrationPeriod() &&
            !mProductionState[ aPeriod - 1 ]->isOperating() && mProductionState[ aPeriod ]->isOperating() )
    {
        delete mProductionState[ aPeriod];
        mProductionState[ aPeriod] = ProductionStateFactory::create(mYear, 0, mFixedOutput, 0.0, aPeriod).release();
    }
}

/*!
 * \brief The "energy" cost is the cost which will be used to determine dispatch order.
 * \details The base class implementation already takes care of most of what we need,
 *          to remove the capital and OM fixed costs.  However we also need to include
 *          any costs associated with emissions etc thus we will tack those on here.
 * \param aRegionName Containing region name.
 * \param aSectorName Containing sector name.
 * \param aPeriod The model period.
 * \return The "energy" cost appropriate to use to determine dispatch.
 */
double CapacityTechnology::getEnergyCost( const string& aRegionName, const string& aSectorName, const int aPeriod ) const {
    return Technology::getEnergyCost( aRegionName, aSectorName, aPeriod ) * mPMultiplier -
        calcSecondaryValue(aRegionName, aPeriod);
}

/*!
 * \brief Calculates the output of the technology.
 * \details Note this specialization differs from the base class implementation
 *          in that aVariableDemand will be used to drive demands and outputs of
 *          this technology.  No further shutdown deciders or production state
 *          adjustments will be made as the dispatch sector will have already taken
 *          these into account when calculating this total annual amount to generate.
 * \param aRegionName Region name.
 * \param aSectorName Sector name, also the name of the product.
 * \param aVariableDemand The actual amount of output to generate, without adjustment.
 * \param aGDP Regional GDP container.
 * \param aPeriod Model period.
 */
void CapacityTechnology::production(const string& aRegionName,
                                    const string& aSectorName,
                                    const double aVariableDemand,
                                    const double aFixedOutputScaleFactor,
                                    const GDP* aGDP,
                                    const int aPeriod)
{
    if( !mProductionState[ aPeriod ]->isOperating() ) {
        return;
    }
    // the aVariableDemand is actually the total production to use to drive
    // demands and outputs
    double actualProduction = aVariableDemand;
    // Calculate input demand.
    mProductionFunction->calcDemand( mInputs, actualProduction, aRegionName, aSectorName,
                                    1, aPeriod, 0, mAlphaZero );
    // calculate outputs and emissions
    calcEmissionsAndOutputs( aRegionName, actualProduction, aGDP, aPeriod );
}

/*!
 * \brief Determine how much energy this technology could provide to meet demands in
 *        the given dispatch segment.
 * \details The load will be determined by the capacity that exists and will be adjusted
 *          by the capacity factor, which may be dispatch segment specific.  In addition
 *          some technologies may not be able to dispatch if they have yet to dispatch
 *          in any of the lower segments and there are not enough remaining to be able
 *          dispatch enough to make it above a minimum capacity factor parameter.
 * \param aRegionName The region in which this technology exists.
 * \param aSectorName The sector in which this technology exists.
 * \param aDispatchSegment The name of the segment we are currently calculating the
 *                         dispatch for.
 * \param aSegmentScaleFactor To be able to convert between load and energy.
 * \param aPercentRemainHours The number of hours yet to be dispatched.
 * \param aPriorDispatch The energy which this technology has dispatched in lower
 *                       dispatch segments.
 * \param aPeriod The model period.
 * \return The maximum energy this technology could provide to the given segment
 *         if it were to dispatch.
 */
double CapacityTechnology::tryDispatch( const string& aRegionName,
                                        const string& aSectorName,
                                        const string& aDispatchSegment,
                                        const double aSegmentScaleFactor,
                                        const double aPercentRemainHours,
                                        const double aPriorDispatch,
                                        const int aPeriod ) const
{
    // First check if this technology has fallen below the minimum capacity factor
    // in which case it will not be able to supply energy generation.  This would
    // be the case if it has yet to dispatch in any of the lower segments and there
    // are not enough remaining hours to make it above the minimum value.
    // Note, this does not apply in calibration periods for simplicity.
    if( aPeriod > scenario->getModeltime()->getFinalCalibrationPeriod() && aPriorDispatch == 0.0 && aPercentRemainHours < ( mMinCapFac ) ) {
        return 0.0;
    }
    
    // Calculate the amount of energy this technology could produce.
    // Note retirement shotdown deciders may come into play here, however profit
    // shutdowns deciders should not exist in mShutdownDeciders as that notion
    // should be taken care of by the dispatch order.
    MarginalProfitCalculator marginalProfitCalc( this );
    double maxProduction = mProductionState[ aPeriod ]->calcProduction( aRegionName,
                                                                        aSectorName,
                                                                        mCapacity * aSegmentScaleFactor,
                                                                        &marginalProfitCalc,
                                                                        aSegmentScaleFactor,
                                                                        mShutdownDeciders,
                                                                        aPeriod );
    // determine the appropriate capacity factor to apply
    double effectiveCapacityFactor;
    auto segCapFac = mSegCapFac.find( aDispatchSegment );
    if( aPeriod <= scenario->getModeltime()->getFinalCalibrationPeriod()) {
        // in the calibration years we back out the effective capacity factor
        // as the output and capacity are calibrated
        effectiveCapacityFactor = mCapacity == 0.0 ? 0.0 : mCalValue->getCalOutput() / mCapacity;
    }
    else if( segCapFac != mSegCapFac.end() ) {
        // we have a segment specific capacity factor so use it
        effectiveCapacityFactor = (*segCapFac).second;
    }
    else {
        // this technology has the same capacity factor regardless of segment
        effectiveCapacityFactor = mCapacityFactor;
    }

    // adjust the max production by the appropriate capacity factor
    return maxProduction * effectiveCapacityFactor;
}

/*!
 * \brief Calculate a scale factor for adjusting the amount of capacity available
 *        in this technology for the purposes of determining how much new investment
 *        should be made.
 * \details We do not want to include a technology's capacity for consideration when
 *          determining how much to invest if it is "cheaper" to invest in something
 *          new than to operate this technology.  This method will be called for each
 *          investment segment which each have their own average cost of new investment.
 * \param aRegionName The region in which this technology exists.
 * \param aSectorName The sector in which this technology exists.
 * \param aNewInvestment What is the average total cost cost to build something new in
 *                       some investment segment.
 * \param aPeriod The model period.
 * \return A scale fraction which may be between zero and one.
 */
double CapacityTechnology::calcInvestmentCapacityScaleFactor( const string& aRegionName,
                                                              const string& aSectorName,
                                                              const double aNewInvestCost,
                                                              const int aPeriod ) const
{
    // the "profit rate" is calculated as the percentage difference between the cost
    // of operating new investment and the cost of operating this technology
    double investCreditAdjCost = getCost(aPeriod) ;//+ 1.25;
    double profitRate = std::max( (aNewInvestCost - investCreditAdjCost)/( fabs(investCreditAdjCost) + util::getVerySmallNumber() ), -1.0);
    // use an instance of the profit shutdown decider to do the actual scale calculation
    return mInvestScaleDecider->calcShutdownCoef( 0, profitRate, aRegionName, aSectorName, mYear, aPeriod );
}

/*!
 * \brief The production state is mostly the same as the base Technology implementation
 *        except instead of using output we set the capacity as the "initial output".
 * \param aPeriod The model period.
 */
void CapacityTechnology::setProductionState( const int aPeriod ) {
    // Check that the state for this period has not already been initialized.
    // Note that this is the case when the same scenario is run multiple times
    // for instance when doing the policy cost calculation.  In which case
    // we must delete the memory to avoid a memory leak.
    if( mProductionState[ aPeriod ] ) {
        delete mProductionState[ aPeriod ];
    }
    
    // use capacity instead of the output in the first year
    double initialOutput = mCapacity;
    
    mProductionState[ aPeriod ] =
        ProductionStateFactory::create( mYear, mLifetimeYears, mFixedOutput,
                                   initialOutput, aPeriod ).release();
}

void CapacityTechnology::doInterpolations(const Technology* aPrevTech, const Technology* aNextTech) {
    Technology::doInterpolations(aPrevTech, aNextTech);

    const CapacityTechnology* prevTech = static_cast<const CapacityTechnology*> (aPrevTech);
    const CapacityTechnology* nextTech = static_cast<const CapacityTechnology*> (aNextTech);

    /*!
    * \pre We were given a valid previous ag production technology.
    */
    assert(prevTech);

    /*!
    * \pre We were given a valid next ag production technology.
    */
    assert(nextTech);
}

void CapacityTechnology::acceptDerived( IVisitor* aVisitor, const int aPeriod ) const {
    // Derived visit.
    aVisitor->startVisitCapacityTechnology( this, aPeriod );
    // End the derived class visit.
    aVisitor->endVisitCapacityTechnology( this, aPeriod );
}

/*!
 * \brief Set the capacity of new vintage technologies.
 * \details The collecting capacity across investment sectors will be handled by
 *          the DispatchSector and it will use this method to set the value.  Note
 *          capacity may only be set for new investment only, otherwise it will be
 *          ignored and a warning generated.
 * \param aCapacity The capacity value to set.
 * \param aPeriod The model period.
 */
void CapacityTechnology::setCapacity( const double aCapacity, const int aPeriod ) {
    if( mProductionState[ aPeriod ]->isNewInvestment() ) {
        mCapacity = aCapacity;
    }
    else {
        ILogger& mainLog = ILogger::getLogger("main_log");
        mainLog.setLevel(ILogger::WARNING);
        mainLog << "Ignoring setCapacity for non-new investment tech: " << mName << ", " << mYear << endl;
    }
}

/*!
 * \brief The getCapacity() method returns mCapacity which corresponds to the capacities of  capacity-technology vintages.
 * \details In other words, this variable contains the capacity of a capacity-technology vintage (typically summed across all investment segments.)
 *          It is noteworthy that the mCapacity is not exactly capacity in GW terms but instead, it corresponds to generation divided by
 *          capacity factor. The capacity factors used for this calculation correspond to investment-segment-specific capacity factors.
 *          Also note that mCapacity does not include retirement functions in it. So this variable represents total capacity without retirements.
 *          Retirements are handled in the calacultion of maxProduction under the CapacityTechnology::tryDispatch() method
 *          using the calcProduction() method which is replicated here to account for retirements. For now, this method considers
 *          only natural retirements.
 * \param aRegionName Name of region.
 * \param aSectorName Name of sector.
 * \param aPeriod Model period.
 */
double CapacityTechnology::getCapacity(const std::string& aRegionName,
                                       const std::string& aSectorName,
                                       const int aPeriod) const
{
    
    if (mProductionState[aPeriod]->isOperating()) {
        // we need to adjust for any natural retirement shutdown deciders
        MarginalProfitCalculator marginalProfitCalc(this);
        double effectiveCapacity = mProductionState[aPeriod]->calcProduction(aRegionName,
                                                                             aSectorName,
                                                                             mCapacity,
                                                                             &marginalProfitCalc,
                                                                             1.0,
                                                                             mShutdownDeciders,
                                                                             aPeriod);
        return effectiveCapacity;
    }
    else {
        return 0.0;
    }
    
}


/*!
 * \brief Add share of an intermittent capacity technology to trial market for capacity credit calculations.
 * \details Calculates the share of intermittent capacity  by first checking if a trial-market-name has been read in.
 *          It uses the argument aAggregateCapacity for the share calculations. This needs to be passed on during the function call.
 *          Note that DispatchSector::calcAggregateCapacity() method performs the aggregate capacity calculations.
 * \param aAggregateCapacity Aggregate capacity of all capacity technology vintages in the containing sector.
 * \param aRegionName The name of the region.
 * \param aPeriod Model period.
 */

void  CapacityTechnology::addCapacityShareToMarket( double aAggregateCapacity, 
                                                    const string& aRegionName, 
                                                    const string& aSectorName,
                                                    const int aPeriod)
{
    // mTrialMarketName is only provided for intermittent capacity and we only
    // need to update the market for them
    if (!mTrialMarketName.empty()) {
        // calculate the capacity share
        mIntermitOutTechRatio = aAggregateCapacity == 0 ? 0.0 :
            getCapacity( aRegionName, aSectorName, aPeriod ) / aAggregateCapacity;
        // update the trial market
        SectorUtils::addToTrialDemand(aRegionName, mTrialMarketName, mIntermitOutTechRatio, aPeriod);
    }
}
