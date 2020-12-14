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
* \file investment_technology.cpp
* \ingroup Objects
* \brief InvestmentTechnology class source file.
* \author Gokul Iyer, Pralit Patel
*/

#include "util/base/include/definitions.h"
#include "technologies/include/investment_technology.h"
#include "containers/include/scenario.h"
#include "util/base/include/xml_helper.h"
#include "technologies/include/iproduction_state.h"
#include "technologies/include/marginal_profit_calculator.h"
#include "technologies/include/ioutput.h"
#include "functions/include/iinput.h"
#include "util/base/include/ivisitor.h"
#include "functions/include/input_capital.h"
#include "sectors/include/capacity_credit_calculator.h"
#include "marketplace/include/marketplace.h"
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
InvestmentTechnology::InvestmentTechnology(const string& aName, const int aYear) :
Technology(aName, aYear)
{
}

// ! Destructor
InvestmentTechnology::~InvestmentTechnology() {
}

//! Clone Function. Returns a deep copy of the current technology.
InvestmentTechnology* InvestmentTechnology::clone() const {
    InvestmentTechnology* clone = new InvestmentTechnology( mName, mYear );
    clone->copy( *this );
    return clone;
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overridden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const string& InvestmentTechnology::getXMLName() const {
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
const string& InvestmentTechnology::getXMLNameStatic() {
    const static string XML_NAME = "investment-technology";
    return XML_NAME;
}

/*!
 * \brief Calculates the output of the technology.
 * \details Calculates the amount of capacity output (in energy terms) based on fuel input and efficiency.
 *          Also do not calculate inputs and emissions (since that will be done in the CapacityTechnology class)
 * \param aRegionName Region name.
 * \param aSectorName Sector name, also the name of the product.
 * \param aVariableDemand Subsector demand for output.
 * \param aGDP Regional GDP container.
 * \param aPeriod Model period.
 */
void InvestmentTechnology::production(const string& aRegionName,
    const string& aSectorName,
    const double aVariableDemand,
    const double aFixedOutputScaleFactor,
    const GDP* aGDP,
    const int aPeriod)
{
    // Can't have a scale factor and positive demand.
    assert(aFixedOutputScaleFactor == 1 || aVariableDemand == 0);

    // Can't have negative variable demand.
    assert(aVariableDemand >= 0 && util::isValidNumber(aVariableDemand));

    // Check for positive variable demand and positive fixed output.
    assert(mFixedOutput == IProductionState::fixedOutputDefault() || util::isEqual(aVariableDemand, 0.0));

    // Check that a state has been created for the period.
    assert(mProductionState[aPeriod]);

    // Early exit optimization to avoid running through the demand function and
    // emissions calculations for non-operating technologies.
    if (!mProductionState[aPeriod]->isOperating()) {
        return;
    }

    // Construct a marginal profit calculator. This allows the calculation of
    // marginal profits to be lazy.
    MarginalProfitCalculator marginalProfitCalc(this);

    // Use the production state to determine output.
    double primaryOutput =
        mProductionState[aPeriod]->calcProduction(aRegionName,
            aSectorName,
            aVariableDemand,
            &marginalProfitCalc,
            aFixedOutputScaleFactor,
            mShutdownDeciders,
            aPeriod);

        // An InvestmentTechnology object should not contribute to energy inputs and/or emissions.
    mOutputs[0]->setPhysicalOutput(primaryOutput, aRegionName, mCaptureComponent, aPeriod);


}

/*!
* \brief Constructor.
* \param aName Technology name.
* \param aYear Technology year.
*/
IntermittentInvestmentTechnology::IntermittentInvestmentTechnology(const string& aName, const int aYear) :
InvestmentTechnology(aName, aYear),
mCapacityCreditCalculator( 0 ),
mMaxCapFac( 0.0 )
{
}

/*!
* \brief Copy the technology paramaters.
* \details Does not copy variables which should get initialized through normal
*          model operations.
* \param aTech Tech InvestmentTechnology to copy.
*/
void IntermittentInvestmentTechnology::copy(const IntermittentInvestmentTechnology& aTech) {
    Technology::copy( aTech );
    mTrialMarketName = aTech.mTrialMarketName;
    mMaxCapFac = aTech.mMaxCapFac;
    
    if (aTech.mCapacityCreditCalculator) {
        delete mCapacityCreditCalculator;
        mCapacityCreditCalculator = aTech.mCapacityCreditCalculator->clone();
    }

}

// ! Destructor
IntermittentInvestmentTechnology::~IntermittentInvestmentTechnology() {
    delete mCapacityCreditCalculator; 
}

//! Parses any input variables specific to derived classes
bool IntermittentInvestmentTechnology::XMLDerivedClassParse(const string& aNodeName, const DOMNode* aCurrNode) {
    bool success = false;
    // Reading in parameters of the capacity-credit function.
    if (aNodeName == CapacityCreditCalculator::getXMLNameStatic()) {
        parseSingleNode(aCurrNode, mCapacityCreditCalculator, new CapacityCreditCalculator);
        success = true;
    }
    else if (aNodeName == "trial-market-name") {
        mTrialMarketName = XMLHelper<string>::getValue(aCurrNode);
        success = true;
    }
    else if(aNodeName == "max-capacity-factor") {
        mMaxCapFac = XMLHelper<double>::getValue(aCurrNode);
        success = true;
    }

    return success;
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overridden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const string& IntermittentInvestmentTechnology::getXMLName() const {
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
const string& IntermittentInvestmentTechnology::getXMLNameStatic() {
    const static string XML_NAME = "intermittent-investment-technology";
    return XML_NAME;
}

//! Clone Function. Returns a deep copy of the current technology.
IntermittentInvestmentTechnology* IntermittentInvestmentTechnology::clone() const {
    IntermittentInvestmentTechnology* clone = new IntermittentInvestmentTechnology( mName, mYear );
    clone->copy( *this );
    return clone;
}

void IntermittentInvestmentTechnology::completeInit(const std::string& aRegionName,
    const std::string& aSectorName,
    const std::string& aSubsectorName,
    const IInfo* aSubsectorInfo,
    ILandAllocator* aLandAllocator)
{
    Technology::completeInit(aRegionName, aSectorName, aSubsectorName, aSubsectorInfo,
        aLandAllocator);
    
    MarketDependencyFinder* depFinder = scenario->getMarketplace()->getDependencyFinder();
    depFinder->addDependency(aSectorName, aRegionName,
                             SectorUtils::getTrialMarketName(mTrialMarketName),
                             aRegionName);
    
    // find the capacity credit input
    const string CAPACITY_CREDIT_NAME = "capacity credit";
    mCapacityCreditInput = mInputs.end();
    mResourceInput = mInputs.end();
    for( InputSetIterator iter = mInputs.begin(); iter != mInputs.end(); ++iter ) {
        if( (*iter)->hasTypeFlag( IInput::RESOURCE )) {
            mResourceInput = iter;
        }
        else if( (*iter)->getName() == CAPACITY_CREDIT_NAME ) {
            mCapacityCreditInput = iter;
        }
    }
    
    // Make some tests for bad inputs
    
    if (mCapacityFactor == 0.0) {
        ILogger& mainLog = ILogger::getLogger("main_log");
        mainLog.setLevel(ILogger::SEVERE);
        mainLog << "Capacity factor not read in for " << getXMLName() << " " << mName << ", " << mYear
                << " in region " << aRegionName << " and sector " << aSectorName << endl;
        abort();
    }
    
    // If the resource input could not be determined then throw error.
    if (mResourceInput == mInputs.end()) {
        ILogger& mainLog = ILogger::getLogger("main_log");
        mainLog.setLevel(ILogger::NOTICE);
        mainLog << getXMLName() << " " << mName << " in sector " << aSectorName
            << " in region " << aRegionName
            << " in vintage " << mYear
            << " could not find the resource input." << endl;
        abort();
    }

    // If capacity-market-price has not been read in then throw error.
    if (mCapacityCreditInput == mInputs.end()) {
        ILogger& mainLog = ILogger::getLogger("main_log");
        mainLog.setLevel(ILogger::NOTICE);
        mainLog << getXMLName() << " " << mName << " in sector " << aSectorName
            << " in region " << aRegionName
            << " in vintage " << mYear
            << " did not read in a capacity credit price." << endl;
    }
    
    if (!mTrialMarketName.empty() && !mCapacityCreditCalculator) {
        ILogger& mainLog = ILogger::getLogger("main_log");
        mainLog.setLevel(ILogger::NOTICE);
        mainLog << getXMLName() << " " << mName << " in sector " << aSectorName
            << " in region " << aRegionName
            << " did not read in a capacity credit calculator for a non-dispatchable technology. Default parameter values will be used" << endl;
    }

        
}

//! write object to xml debugging output stream
void IntermittentInvestmentTechnology::toDebugXMLDerived(const int aPeriod, ostream& aOut, Tabs* aTabs) const
{
    XMLWriteElement(mTrialMarketName, "trial-market-name", aOut, aTabs);
    if (mCapacityCreditCalculator) {
            mCapacityCreditCalculator->toDebugXML(aPeriod, aOut, aTabs);
    }
}

void IntermittentInvestmentTechnology::initCalc(const string& aRegionName,
    const string& aSectorName,
    const IInfo* aSubsectorInfo,
    const Demographic* aDemographics,
    PreviousPeriodInfo& aPrevPeriodInfo,
    const int aPeriod)
{
    Technology::initCalc(aRegionName, aSectorName, aSubsectorInfo,
        aDemographics, aPrevPeriodInfo, aPeriod);
}
  
/*!
 * \brief Adjust capacity payments before calculating the cost of the technology.
 * \details The CapacityCreditCalculator:: getCapacityPayment method is called to
 *          adjust the coefficient on the capacity credit input before using the
 *          base class method to do the cost calculation.
 * \param aRegionName Region name.
 * \param aSectorName Sector name, also the name of the product.
 * \param aPeriod The model period.
 */
void IntermittentInvestmentTechnology::calcCost(const string& aRegionName,
                                    const string& aSectorName,
                                    const int aPeriod)
{
    
    if( mProductionState[aPeriod]->isOperating() && mCapacityCreditCalculator && mCapacityCreditInput != mInputs.end() ) {
        // get the actual capacity factor which is 1 - price of the resource input
        double actualCF = std::max(std::min(1.0 - (*mResourceInput)->getPrice(aRegionName, aPeriod), mMaxCapFac), util::getSmallNumber());
        // since we the technology capacity factor has already been included in the levelized
        // costs we will instead adjust the coefficient for those inputs by the relative difference
        // between the technology capacity factor and the actual
        double capFacAdj = mCapacityFactor / actualCF;
        
        // For intermittent technologies, the method calls the CapacityCreditCalculator::getCapacityCredit method
        // which is used to calculate the capacity credit (same as capacityPaymentFraction, 0-1) as a function of renewable share
        // in the capacity market (typically grid region).
        double capacityCreditAdj = mCapacityCreditCalculator ? mCapacityCreditCalculator->getCapacityCredit( aRegionName, mTrialMarketName, aPeriod ) : 1.0;
        
        for(auto input : mInputs) {
            if(input == *mResourceInput) {
                // the resource input price is just the capacity factor and therefore
                // should not be included directly in the technology cost
                input->setCoefficient(0.0, aPeriod);
            }
            else if(input == *mCapacityCreditInput) {
                // for the capacity credit we need to adjust for both the actual capacity
                // factor but also the intermittent penalty
                input->setCoefficient( capacityCreditAdj * capFacAdj, aPeriod);
            }
            else if(!input->hasTypeFlag(IInput::OM_VAR)) {
                // for all other non-fixed inputs (i.e. capacity factor does not impact
                // its levelized cost) we simply reset the coefficient
                input->setCoefficient( capFacAdj, aPeriod);
            }
        }
    }
    
    Technology::calcCost( aRegionName, aSectorName, aPeriod );
}

double IntermittentInvestmentTechnology::getCapacityFactor() const {
    int techPeriod = scenario->getModeltime()->getyr_to_per(mYear);
    for(auto input : mInputs) {
        if(input->hasTypeFlag(IInput::CAPITAL) && input != *mResourceInput && input != *mCapacityCreditInput) {
            return mCapacityFactor / input->getCoefficient(techPeriod);
        }
    }
    return mCapacityFactor;
}

void IntermittentInvestmentTechnology::doInterpolations(const Technology* aPrevTech, const Technology* aNextTech) {
    Technology::doInterpolations(aPrevTech, aNextTech);

    const InvestmentTechnology* prevTech = static_cast<const InvestmentTechnology*> (aPrevTech);
    const InvestmentTechnology* nextTech = static_cast<const InvestmentTechnology*> (aNextTech);

    //     \pre We were given a valid previous InvestmentTechnology object.
    
    assert(prevTech);

    // \pre We were given a valid next InvestmentTechnology object.
    
    assert(nextTech);
}

void IntermittentInvestmentTechnology::acceptDerived( IVisitor* aVisitor, const int aPeriod ) const {
    // Derived visit.
    aVisitor->startVisitInvestmentTechnology( this, aPeriod );
    // End the derived class visit.
    aVisitor->endVisitInvestmentTechnology( this, aPeriod );
}



