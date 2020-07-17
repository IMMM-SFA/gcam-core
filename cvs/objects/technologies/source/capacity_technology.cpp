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
//#include "emissions/include/aghg.h"
#include "containers/include/scenario.h"
#include "util/base/include/xml_helper.h"
//#include "marketplace/include/marketplace.h"
#include "containers/include/iinfo.h"
#include "technologies/include/ical_data.h"
#include "technologies/include/iproduction_state.h"
#include "technologies/include/production_state_factory.h"
#include "technologies/include/marginal_profit_calculator.h"
#include "technologies/include/ioutput.h"
#include "technologies/include/generic_output.h"
#include "util/base/include/ivisitor.h"
//#include "containers/include/market_dependency_finder.h"
//#include "sectors/include/sector_utils.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

/*!
* \brief Constructor.
* \param aName Technology name.
* \param aYear Technology year.
*/
CapacityTechnology::CapacityTechnology(const string& aName, const int aYear) :
Technology(aName, aYear)
{
    mCapacityFactor = 0.1;
}

/*!
* \brief Copy the technology paramaters.
* \details Does not copy variables which should get initialized through normal
*          model operations.
* \param aTech Tech CapacityTechnology to copy.
*/
void CapacityTechnology::copy(const CapacityTechnology& aTech) {
    Technology::copy( aTech );
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
	return success;
}

//! write object to xml output stream
void CapacityTechnology::toInputXMLDerived(ostream& aOut, Tabs* aTabs) const {
    XMLWriteElement(mCapacity, "capacity", aOut, aTabs);
}

//! write object to xml output stream
void CapacityTechnology::toDebugXMLDerived(const int aPeriod, ostream& aOut, Tabs* aTabs) const {
	XMLWriteElement(mCapacity, "capacity", aOut, aTabs);
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
	// Note: Technology::completeInit() loops through the outputs.
	//       Therefore, if any of the outputs need the land allocator,
	//       the call to Technology::completeInit() must come afterwards
	Technology::completeInit(aRegionName, aSectorName, aSubsectorName, aSubsectorInfo,
		aLandAllocator);
    
    // replace the primary output with a generic output (does not add supply to market)
    delete mOutputs[ 0 ];
    mOutputs[ 0 ] = new GenericOutput( aSectorName );
    
    initTechVintageVector();

	// Make some tests for bad inputs
	if (mCapacityFactor == 0.0) {
		ILogger& mainLog = ILogger::getLogger("main_log");
		mainLog.setLevel(ILogger::SEVERE);
		mainLog << "Capacity factor not set for technology " << mName << ", " << mYear
			    << " in region " << aRegionName << " and sector " << aSectorName << endl;
		//abort();
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

    if( aPeriod < (scenario->getModeltime()->getmaxper()-1) ) {
        setProductionState( aPeriod + 1 );
    }
}

/*! \brief Calculates the output of the technology.
* \details Calculates the amount of current ag output based on the amount
*          land and it's yield.
* \param aRegionName Region name.
* \param aSectorName Sector name, also the name of the product.
* \param aVariableDemand Subsector demand for output.
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
	// The production is exactly the same as the technology except the variable demand
	// is given in terms of capacity so we convert that to energy to drive
	// Technology::production
    if( aFixedOutputScaleFactor == -1.0 ) {
        double actualProduction = aVariableDemand;
        if( !mProductionState[ aPeriod ]->isOperating() ) {
            return;
        }
        // Calculate input demand.
        mProductionFunction->calcDemand( mInputs, actualProduction, aRegionName, aSectorName,
                                        1, aPeriod, 0, mAlphaZero );
        
        calcEmissionsAndOutputs( aRegionName, actualProduction, aGDP, aPeriod );
    } else if( aFixedOutputScaleFactor == -2.0 ) {
        mCapacity = aVariableDemand;
    }
}

double CapacityTechnology::tryDispatch( const string& aRegionName,
                                        const string& aSectorName,
                                        const string& aDemandSegment,
                                        const double aVariableDemand,
                                        const double aSegmentScaleFactor,
                                        const int aPeriod )
{
    double effectiveCapacityFactor = mCapacityFactor;
    auto segCapFac = mSegCapFac.find( aDemandSegment );
    if( aPeriod <= scenario->getModeltime()->getFinalCalibrationPeriod()) {
        effectiveCapacityFactor = mCapacity == 0.0 ? 0.0 : mCalValue->getCalOutput() / mCapacity;
    }
    else if( segCapFac != mSegCapFac.end() ) {
        effectiveCapacityFactor = (*segCapFac).second;
    }
    
    MarginalProfitCalculator marginalProfitCalc( this );
    double maxProduction = mProductionState[ aPeriod ]->calcProduction( aRegionName,
                                                                        aSectorName,
                                                                        mCapacity * effectiveCapacityFactor * aSegmentScaleFactor,
                                                                        &marginalProfitCalc,
                                                                        aSegmentScaleFactor,
                                                                        mShutdownDeciders,
                                                                        aPeriod );

    return maxProduction;
}

void CapacityTechnology::setProductionState( const int aPeriod ) {
    // Check that the state for this period has not already been initialized.
    // Note that this is the case when the same scenario is run multiple times
    // for instance when doing the policy cost calculation.  In which case
    // we must delete the memory to avoid a memory leak.
    if( mProductionState[ aPeriod ] ) {
        delete mProductionState[ aPeriod ];
    }
    
    double initialOutput = mCapacity * mCapacityFactor;
    
    mProductionState[ aPeriod ] =
        ProductionStateFactory::create( mYear, mLifetimeYears, mFixedOutput,
                                   initialOutput, aPeriod ).release();
}

double CapacityTechnology::getCalibrationOutput( const bool aHasRequiredInput,
                                                 const string& aRequiredInput,
                                                 const int aPeriod ) const
{
    double techCalOutput = Technology::getCalibrationOutput( aHasRequiredInput, aRequiredInput, aPeriod );
    return techCalOutput ;//== -1 ? techCalOutput : techCalOutput / mCapacityFactor;
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
