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
#include "functions/include/input_capital.h"
#include "containers/include/market_dependency_finder.h"
#include "sectors/include/sector_utils.h"
#include "sectors/include/capacity_credit_calculator.h"
//#include "sectors/include/dispatch_sector.h"

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
    double mCapacityFactor = 0.1;
	double mCapacityMarketPrice = 0; 
	bool mIsDispatchable = 1;
}

/*!
* \brief Copy the technology paramaters.
* \details Does not copy variables which should get initialized through normal
*          model operations.
* \param aTech Tech InvestmentTechnology to copy.
*/
void InvestmentTechnology::copy(const InvestmentTechnology& aTech) {
    Technology::copy( aTech );
}

// ! Destructor
InvestmentTechnology::~InvestmentTechnology() {
}

//! Parses any input variables specific to derived classes
bool InvestmentTechnology::XMLDerivedClassParse(const string& aNodeName, const DOMNode* aCurrNode) {
    bool success = false;
    if( aNodeName == "capacity-market-price" ) {
        mCapacityMarketPrice = XMLHelper<double>::getValue( aCurrNode );
        success = true;
    }
    else if( aNodeName == "is-dispatchable" ) {
		mIsDispatchable = XMLHelper<bool>::getValue(aCurrNode);
        success = true;
    }
	//GI: Reading in parameters of the capacity-credit function. 
	else if (aNodeName == CapacityCreditCalculator::getXMLNameStatic()) {
		parseSingleNode(aCurrNode, mCapacityCreditCalculator, new CapacityCreditCalculator);
		success = true;
	}

	return success;
}

//! write object to xml output stream
//GI: I don't think we need this. Commenting for now. PP: Is that correct?
//void InvestmentTechnology::toInputXMLDerived(ostream& aOut, Tabs* aTabs) const {
//    XMLWriteElement(mCapacity, "capacity", aOut, aTabs);
//}

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

//! Clone Function. Returns a deep copy of the current technology.
InvestmentTechnology* InvestmentTechnology::clone() const {
    InvestmentTechnology* clone = new InvestmentTechnology( mName, mYear );
    clone->copy( *this );
    return clone;
}

void InvestmentTechnology::completeInit(const std::string& aRegionName,
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
	// GI: I don't know if we need this for the InvestmentTechnology class
    delete mOutputs[ 0 ];
    mOutputs[ 0 ] = new GenericOutput( aSectorName );
    
    initTechVintageVector();

	// Make some tests for bad inputs
	
	if (mCapacityFactor == 0.0) {
		ILogger& mainLog = ILogger::getLogger("main_log");
		mainLog.setLevel(ILogger::SEVERE);
		mainLog << "Capacity factor not read in for investment- technology " << mName << ", " << mYear
			    << " in region " << aRegionName << " and sector " << aSectorName << endl;
		//abort();
	}

	// If capacity-market-price has not been read in then throw error.
	else if (!mCapacityMarketPrice) {
		ILogger& mainLog = ILogger::getLogger("main_log");
		mainLog.setLevel(ILogger::NOTICE);
		mainLog << "Investment Technology " << mName << " in sector " << aSectorName
			<< " in region " << aRegionName
			<< " did not read in a capacity market price. Capacity payments will default to zero. " << endl;
	}
	
	else if (!mIsDispatchable) {
		ILogger& mainLog = ILogger::getLogger("main_log");
		mainLog.setLevel(ILogger::NOTICE);
		mainLog << "Investment Technology " << mName << " in sector " << aSectorName
			<< " in region " << aRegionName
			<< " did not read in an is-dispatchable bool. Investment technology object will be assumed to be dispatchable. " << endl;
	};

		
}

//! write object to xml debugging output stream
void InvestmentTechnology::toDebugXMLDerived(const int aPeriod, ostream& aOut, Tabs* aTabs) const
{
	XMLWriteElement(mCapacityMarketPrice, "capacity-market-price", aOut, aTabs);
	XMLWriteElement(mIsDispatchable, "is-dispatchable", aOut, aTabs);
	
	if (mCapacityPayment) {
		XMLWriteElement(mCapacityPayment, "capacity-payment", aOut, aTabs);
	}
}

void InvestmentTechnology::initCalc(const string& aRegionName,
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
* \details Calculates the amount of capacity output (in energy terms) based on fuel input and efficiency. 
			Also do not calculate inputs and emissions (since that will be done in the CapacityTechnology class)
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
  
/*!Performing capacity credit calculation. We loop over all InvestmentTechnology objects. 
	For each InvestmentTechnology object, the CapacityCreditCalculator:: getCapacityPayment method is called
	to apply the credit to the non-energy cost component of the investment-technology. 
	Note that ideally we should be applying this to the capital-overnight component but that's not possible for now.
	We're keeping it simple and applying the credit in levelized cost terms. 
	*/

void InvestmentTechnology::calcCost(const string& aRegionName,
	const string& aSectorName,
	const int aPeriod) {
	
	// A Technology can only calculate costs if it is operating
   // Note that attempted to retrieve a cost when the technology is not
   // operating will cause an abort.
	if (mProductionState[aPeriod]->isOperating()) {
		// Note we now allow costs in any sector to be <= 0.  If,
		// however, you are using the relative cost logit, costs will be
		// clamped on the low end for market share purposes (not for
		// other purposes, though).

		double cost = getTotalInputCost(aRegionName, aSectorName, aPeriod) 
			* mPMultiplier 
			- calcSecondaryValue(aRegionName, aPeriod);

	/*Obtain capacity payments in $/kW using the getCapacityPayment() method and levelize those to $/GJ using 
	technology-specific capacity factors. 
	TODO: DO these calculations as part of the input-capital class*/

		// Initialize a constant to convert GJ to kWh
		const double KWH_TO_GJ = 0.0036;
		
		// Initialize a constant for number of hours in a year.
		const int HOURS_PER_YEAR = 8760;
		
		// Initialize a constant for the fixed charge out rate or capital recovery factor.
		// Assuming an FCR of 0.13 which is prevalent in GCAM. 
		// Ideally we want to use the FCR that's being read in in the input-capital object but that's complicated.
		
		const double FCR = 0.13;

		double CapacityPayment_USD_GJ = getCapacityPayment(aRegionName, aSectorName, aPeriod) 
																	* FCR / mCapacityFactor/ HOURS_PER_YEAR/ KWH_TO_GJ;
		
		cost =- CapacityPayment_USD_GJ;
		
		mCosts[aPeriod] = cost;

		assert(util::isValidNumber(mCosts[aPeriod]));
	}
	
	
}
		  

/* The InvestmentTechnology::getCapacityPayment method returns the capacity payments to be deducted from 
	technology costs. The units are in $/kW. 
	 calls the CapacityCreditCalculator::getCapacityCredit method which is used to calculate 
	the capacity credit (0-1) as a function of renewable share for renewable technologies 
 */

double InvestmentTechnology::getCapacityPayment(const string& aRegionName,
	const string& aSectorName,
	const int aPeriod) {

	if (mIsDispatchable) {
		mCapacityPayment = mCapacityMarketPrice;
	}
		
	else {
		
		// double capacityPaymentFraction = mCapacityCreditCalculator -> getCapacityCredit(aRegionName, aSectorName, aPeriod);

		double capacityPaymentFraction = dynamic_cast<CapacityCreditCalculator*>(mCapacityCreditCalculator)->getCapacityCredit(aRegionName, 
																										  aSectorName, 
																										  aPeriod);
		mCapacityPayment = mCapacityMarketPrice * capacityPaymentFraction;

		
		// PP sugested reading in capacity-market-price as a input-capital. But that might confuse users - especially if we were to read it in as  "capital-overnight". 
		// Instead, GI thinks it might just be simpler and more intuitive to either: i.) create a new input class or 
		// ii.) just read in capacity - market - price along with the investment - technology object and include it in the costs within the calcCost() method.
		// FOr now, GI started along second option above.
		// mInputs[mCapCreditInputIndex]->setCoefficient(capacityPaymentFraction, aPeriod);
	}
		   	
	return mCapacityPayment;
}

// Probably don't need this for investment technology class. 
/*
void InvestmentTechnology::setProductionState( const int aPeriod ) {
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
*/


/*// Probably don't need this for investment technology class since it will be carried forward from the parent Technology class.
double InvestmentTechnology::getCalibrationOutput( const bool aHasRequiredInput,
                                                 const string& aRequiredInput,
                                                 const int aPeriod ) const
{
    double techCalOutput = Technology::getCalibrationOutput( aHasRequiredInput, aRequiredInput, aPeriod );
    return techCalOutput ;//== -1 ? techCalOutput : techCalOutput / mCapacityFactor;
}
*/


void InvestmentTechnology::doInterpolations(const Technology* aPrevTech, const Technology* aNextTech) {
	Technology::doInterpolations(aPrevTech, aNextTech);

	const InvestmentTechnology* prevTech = static_cast<const InvestmentTechnology*> (aPrevTech);
	const InvestmentTechnology* nextTech = static_cast<const InvestmentTechnology*> (aNextTech);

	//	 \pre We were given a valid previous InvestmentTechnology object.
	
	assert(prevTech);

	// \pre We were given a valid next InvestmentTechnology object.
	
	assert(nextTech);
}
//GI: Do we need this or will this be carried forward by parent class?
void InvestmentTechnology::acceptDerived( IVisitor* aVisitor, const int aPeriod ) const {
    // Derived visit.
    aVisitor->startVisitInvestmentTechnology( this, aPeriod );
    // End the derived class visit.
    aVisitor->endVisitInvestmentTechnology( this, aPeriod );
}



