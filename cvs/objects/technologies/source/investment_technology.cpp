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

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

/*!
* \brief Constructor.
* \param aName Technology name.
* \param aYear Technology year.
*/
InvestmentTechnology::InvestmentTechnology(const string& aName, const int aYear) :
Technology(aName, aYear),
mCapacityCreditCalculator( 0 )
{
}

/*!
* \brief Copy the technology paramaters.
* \details Does not copy variables which should get initialized through normal
*          model operations.
* \param aTech Tech InvestmentTechnology to copy.
*/
void InvestmentTechnology::copy(const InvestmentTechnology& aTech) {
    Technology::copy( aTech );
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
	// Reading in parameters of the capacity-credit function.
	if (aNodeName == CapacityCreditCalculator::getXMLNameStatic()) {
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
    
    // find the capacity credit input
    const string CAPACITY_CREDIT_NAME = "capacity credit";
    mCapacityCreditInput = mInputs.end();
    for( InputSetIterator iter = mInputs.begin(); mCapacityCreditInput == mInputs.end() && iter != mInputs.end(); ++iter ) {
        if( (*iter)->getName() == CAPACITY_CREDIT_NAME ) {
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
void InvestmentTechnology::toDebugXMLDerived(const int aPeriod, ostream& aOut, Tabs* aTabs) const
{
	XMLWriteElement(mTrialMarketName, "trial-market-name", aOut, aTabs);
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
                                    const int aPeriod)
{
    
    if( mProductionState[aPeriod]->isOperating() && mCapacityCreditCalculator && mCapacityCreditInput != mInputs.end() ) {
        // For intermittent technologies, the method calls the CapacityCreditCalculator::getCapacityCredit method
        // which is used to calculate the capacity credit (same as capacityPaymentFraction, 0-1) as a function of renewable share
        // in the capacity market (typically grid region).
        double capacityCreditAdj = mCapacityCreditCalculator->getCapacityCredit( aRegionName, mTrialMarketName, aPeriod );
        (*mCapacityCreditInput)->setCoefficient( capacityCreditAdj, aPeriod );
    }
    
    Technology::calcCost( aRegionName, aSectorName, aPeriod );
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



