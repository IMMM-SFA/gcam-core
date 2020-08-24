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
 * \file capacity_credit_calculator.cpp
 * \ingroup Objects
 * \brief CapacityCreditCalculator class source file.
 * \author Gokul Iyer, Pralit Patel
 */

#include "util/base/include/definitions.h"
#include <string>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
#include <math.h>

#include "sectors/include/capacity_credit_calculator.h"
#include "util/base/include/util.h"
#include "util/base/include/xml_helper.h"
#include "sectors/include/sector_utils.h"
#include "marketplace/include/marketplace.h"

using namespace std;
using namespace xercesc;


/*!
 * \brief Constructor.
 */
CapacityCreditCalculator::CapacityCreditCalculator()
{
     mCapacityCreditMax = 0.4;
    mCapacityCreditMin = 0.15;
    mSteepness = 40.0;
    mXmid = 0.075;
}

// Documentation is inherited.
CapacityCreditCalculator* CapacityCreditCalculator::clone() const {
    CapacityCreditCalculator* clone = new CapacityCreditCalculator();
    clone->mCapacityCreditMax = mCapacityCreditMax;
    clone->mCapacityCreditMin = mCapacityCreditMin;
    clone->mSteepness = mSteepness;
    clone->mXmid = mXmid;
    
    return clone;
}

// Documentation is inherited.
bool CapacityCreditCalculator::isSameType( const std::string& aType ) const {
    return aType == getXMLNameStatic();
}

// Documentation is inherited.
const string& CapacityCreditCalculator::getName() const {
    return getXMLNameStatic();
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME. This way
* the tag is always consistent for both read-in and output and can be easily
* changed. The "==" operator that is used when parsing, required this second
* function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME as a static.
*/
const string& CapacityCreditCalculator::getXMLNameStatic() {
    const static string XML_NAME = "capacity-credit-calculator";
    return XML_NAME;
}

// Documentation is inherited.
bool CapacityCreditCalculator::XMLParse( const xercesc::DOMNode* node ){
    /*! \pre Assume we are passed a valid node. */
    assert( node );

    const xercesc::DOMNodeList* nodeList = node->getChildNodes();
    for( unsigned int i = 0; i < nodeList->getLength(); i++ ) {
        const xercesc::DOMNode* curr = nodeList->item( i );
        if( curr->getNodeType() != xercesc::DOMNode::ELEMENT_NODE ){
            continue;
        }
        const string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );
        if( nodeName == "capacity-credit-max" ){
			mCapacityCreditMax = XMLHelper<double>::getValue( curr );
            // TODO: Correct values above 1 or below 0. Need completeInit.
        }
        else if( nodeName == "capacity-credit-min" ) {
			mCapacityCreditMin = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "steepness" ) {
            mSteepness = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "x-mid" ) {
            mXmid = XMLHelper<double>::getValue( curr );
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Unknown tag " << nodeName << " encountered while processing "
                    << getXMLNameStatic() << endl;
        }
    }

    // TODO: Handle success and failure better.
    return true;
}

// Documentation is inherited.
void CapacityCreditCalculator::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteElement(mCapacityCreditMax, "capacity-credit-max", aOut, aTabs );
    XMLWriteElement(mCapacityCreditMin, "capacity-credit-min", aOut, aTabs );
    XMLWriteElement( mSteepness, "steepness", aOut, aTabs );
    XMLWriteElement( mXmid, "x-mid", aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

// Documentation is inherited.
void CapacityCreditCalculator::initCalc( const IInfo* aTechInfo ) {
    // No information needs to be passed in
}


/*!
 * \brief Calculate the capacity credit for the intermittent resource within the
 *        electricity sector.
 * \details Calculates the capacity credit (or capacity value; 0-1) as a function of 
			share of capacity of the intermittent resource within
 *          the electricity sector. This is determined using trial values for
 *          the intermittent technologies and electricity sector capacity. 
 * \param aRegion Name of the containing region.
 * \param aSector The name of the sector for which capacity credits are being calculated.
 * \param aPeriod Model period.
 * \return Capacity credit.
 */

double CapacityCreditCalculator::getCapacityCredit (const string& aRegion,
													const string& aSector,
													const int aPeriod)
   
	{
    // Preconditions
    assert( !aRegion.empty() );
    
    // ensure we get a valid (between zero and one) share back from the solver
    double renewElecShare = std::max(std::min( SectorUtils::getTrialSupply( aRegion, aSector, aPeriod ), 1.0 ), 0.0);
	
	// Calculate the capacity credit at this share of the total.
	double capacityCredit = mCapacityCreditMin + (mCapacityCreditMax - mCapacityCreditMin) * 1 / (1 + exp(mSteepness *(renewElecShare - mXmid)));

	// Capacity Credit must be between 0 and 1 inclusive.
	assert(capacityCredit >= 0 && capacityCredit <= 1);
	
	return capacityCredit;
}
    

