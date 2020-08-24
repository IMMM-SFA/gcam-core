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
#include "util/base/include/ivisitor.h"
#include "functions/include/input_capital.h"
#include "sectors/include/capacity_credit_calculator.h"

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
     mCapacityFactor = 0.1;
	 mCapacityMarketPrice = 0;
	 mCapacityCreditCalculator = 0;
}

/*!
* \brief Copy the technology paramaters.
* \details Does not copy variables which should get initialized through normal
*          model operations.
* \param aTech Tech InvestmentTechnology to copy.
*/
void InvestmentTechnology::copy(const InvestmentTechnology& aTech) {
    Technology::copy( aTech );
	mCapacityMarketPrice = aTech.mCapacityMarketPrice;
	mTrialMarketName = aTech.mTrialMarketName;
	
	if (aTech.mCapacityCreditCalculator) {
		delete mCapacityCreditCalculator;
		mCapacityCreditCalculator = aTech.mCapacityCreditCalculator->clone();
	}

}

// ! Destructor
InvestmentTechnology::~InvestmentTechnology() {
	delete mCapacityCreditCalculator; 
}

//! Parses any input variables specific to derived classes
bool InvestmentTechnology::XMLDerivedClassParse(const string& aNodeName, const DOMNode* aCurrNode) {
    bool success = false;
    if( aNodeName == "capacity-market-price" ) {
        mCapacityMarketPrice = XMLHelper<double>::getValue( aCurrNode );
        success = true;
    }
	// Reading in parameters of the capacity-credit function.
	else if (aNodeName == CapacityCreditCalculator::getXMLNameStatic()) {
		parseSingleNode(aCurrNode, mCapacityCreditCalculator, new CapacityCreditCalculator);
		success = true;
	}
	else if (aNodeName == "trial-market-name") {
		mTrialMarketName = XMLHelper<string>::getValue(aCurrNode);
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
	Technology::completeInit(aRegionName, aSectorName, aSubsectorName, aSubsectorInfo,
		aLandAllocator);
    
	// Make some tests for bad inputs
	
	if (mCapacityFactor == 0.0) {
		ILogger& mainLog = ILogger::getLogger("main_log");
		mainLog.setLevel(ILogger::SEVERE);
		mainLog << "Capacity factor not read in for " << getXMLName() << " " << mName << ", " << mYear
			    << " in region " << aRegionName << " and sector " << aSectorName << endl;
		//abort();
	}

	// If capacity-market-price has not been read in then throw error.
	if (mCapacityMarketPrice == 0.0) {
		ILogger& mainLog = ILogger::getLogger("main_log");
		mainLog.setLevel(ILogger::NOTICE);
		mainLog << getXMLName() << " " << mName << " in sector " << aSectorName
			<< " in region " << aRegionName
			<< " in vintage " << mYear
			<< " did not read in a capacity market price. Capacity payments will default to zero. " << endl;
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
void InvestmentTechnology::toDebugXMLDerived(const int aPeriod, ostream& aOut, Tabs* aTabs) const
{
	XMLWriteElement(mCapacityMarketPrice, "capacity-market-price", aOut, aTabs);
	XMLWriteElement(mTrialMarketName, "trial-market-name", aOut, aTabs);
	
	if (mCapacityPayment) {
		XMLWriteElement(mCapacityPayment, "capacity-payment", aOut, aTabs);
	}
	if (mCapacityCreditCalculator) {
			mCapacityCreditCalculator->toDebugXML(aPeriod, aOut, aTabs);
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
  
/*!Applying capacity payments to levelized costs of investment technologies. 
	The CapacityCreditCalculator:: getCapacityPayment method is called to apply the payment to the levelized costs 
	of the investment-technology (which is in turn used for logit calculations). 
	Note that ideally we should be applying this to the capital-overnight component but that could be messy.
*/

void InvestmentTechnology::calcCost(const string& aRegionName,
	const string& aSectorName,
	const int aPeriod) {
    
    Technology::calcCost( aRegionName, aSectorName, aPeriod );
	
	// A Technology can only calculate costs if it is operating
   // Note that attempted to retrieve a cost when the technology is not
   // operating will cause an abort.
	if (mProductionState[aPeriod]->isOperating()) {

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

		double capacityPayment_USD_GJ = getCapacityPayment(aRegionName, aSectorName, aPeriod)
																	* FCR / mCapacityFactor/ HOURS_PER_YEAR/ KWH_TO_GJ;
		
		// Adjust levelized costs with capacity payments.
		
		mCosts[aPeriod] -= capacityPayment_USD_GJ;

		assert(util::isValidNumber(mCosts[aPeriod]));
	}
	
	
}
		  

/*!
 * \brief The InvestmentTechnology::getCapacityPayment method returns the capacity payments to be deducted from 
			technology costs. The units are in $/kW..
 * \details For dispatchable technologies, capacity payment is equal to the capacity market price. 
			FOr intermittent technologies, the method calls the CapacityCreditCalculator::getCapacityCredit method 
			which is used to calculate the capacity credit (same as capacityPaymentFraction, 0-1) as a function of renewable share 
			in the capacity market (typically grid region).
* \param aRegion Name of the containing region.
 * \param aSector The name of the sector for which capacity credits are being calculated.
 * \param aPeriod Model period.
 * \return Capacity Payments in $/kW.
 NOTE:  PP sugested reading in capacity-market-price within the InputCapital class. But that might confuse users - especially 
		if we were to read it in as  "capital-overnight". Instead, GI thinks it might just be simpler and more intuitive to 
		either: i.) create a new input class or ii.) just read in capacity - market - price along with the investment - technology 
		object and include it in the costs within the calcCost() method. For now, GI started along second option above.
 */


double InvestmentTechnology::getCapacityPayment(const string& aRegionName,
	const string& aSectorName,
	const int aPeriod) {

	if (mTrialMarketName.empty()) {
		mCapacityPayment = mCapacityMarketPrice;
	}
		
	else {
		
		

		double capacityPaymentFraction = dynamic_cast<CapacityCreditCalculator*>(mCapacityCreditCalculator)->getCapacityCredit(aRegionName, 
																										  mTrialMarketName, 
																										  aPeriod);
		mCapacityPayment = mCapacityMarketPrice * capacityPaymentFraction;

		
			}
		   	
	return mCapacityPayment;
}




void InvestmentTechnology::doInterpolations(const Technology* aPrevTech, const Technology* aNextTech) {
	Technology::doInterpolations(aPrevTech, aNextTech);

	const InvestmentTechnology* prevTech = static_cast<const InvestmentTechnology*> (aPrevTech);
	const InvestmentTechnology* nextTech = static_cast<const InvestmentTechnology*> (aNextTech);

	//	 \pre We were given a valid previous InvestmentTechnology object.
	
	assert(prevTech);

	// \pre We were given a valid next InvestmentTechnology object.
	
	assert(nextTech);
}

void InvestmentTechnology::acceptDerived( IVisitor* aVisitor, const int aPeriod ) const {
    // Derived visit.
    aVisitor->startVisitInvestmentTechnology( this, aPeriod );
    // End the derived class visit.
    aVisitor->endVisitInvestmentTechnology( this, aPeriod );
}



