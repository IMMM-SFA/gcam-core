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


#ifndef _CAPACITY_CREDIT_CALCULATOR_H_
#define _CAPACITY_CREDIT_CALCULATOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*!
 * \file capacity_credit_calculator.h
 * \ingroup Objects
 * \brief The CapacityCreditCalculator class header file.
 * \author Gokul Iyer
 */

#include <string>
#include "sectors/include/ibackup_calculator.h"

// Forward declaration
class IInfo;

/*!
 * \ingroup Objects
 * \brief The Capacity credit calculator for intermittent technologies.
 * \details Calculates the capacity credit (or capacity value; 0-1) as a function of 
			share of capacity of the intermittent resource within
 *          the electricity sector. 
 *
 *          functional form of capacity credit as a function of renewshare is
 *          
 *          mCapacityCredit = mCapacityCreditMin + (mCapacityCreditMax - mCapacityCreditMin) * 1/ (1+ exp(mSteepness *(renewElecShare-mXmid)))
 *                  where  mCapacityCreditMin is the minimum capacity credit (defaults to 0.15)
 *							mCapacityCreditMax is the maximum capcity credit (defaults to 0.4)
 							mSteepness and mXmid are shape parameters
 *                          mXmid is the value of the share at which mCapacityCredit = 0.5*(mCapacityCreditMax - mCapacityCreditMin)
 *                                
 *
 *         
 *          <b>XML specification for CapacityLimitBackupCalculator</b>
 *          - XML name: \c capacity-credit-calculator
 *          - Contained by: Technology
 *          - Parsing inherited from class: None
 *          - Attributes: None
 *          - Elements:
 *              - \c capacity-credit-min CapacityCreditCalculator::mCapacityCreditMin
				- \c capacity-credit-max CapacityCreditCalculator::mCapacityCreditMax
 *				- \c steepness CapacityCreditCalculator::mSteepness
 				- \c x-mid CapacityCreditCalculator::mXmid
				
				* \author Gokul Iyer, Pralit Patel
 */
class CapacityCreditCalculator{
public:
    virtual CapacityCreditCalculator* clone() const;
    virtual bool isSameType( const std::string& aType ) const;
    virtual const std::string& getName() const;
    virtual bool XMLParse( const xercesc::DOMNode* aNode );
    virtual void toDebugXML( const int aPeriod, std::ostream& aOut, Tabs* aTabs ) const;
    virtual void initCalc( const IInfo* aTechInfo );
    
    virtual double CapacityCreditCalculator::getCapacityCredit(const std::string& aRegion,
																const std::string& aSector,
																const int aPeriod) ;
	
	static const std::string& getXMLNameStatic();

	CapacityCreditCalculator();
  
protected:
	
	   

       
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
	DEFINE_DATA(
		DEFINE_SUBCLASS_FAMILY( CapacityCreditCalculator ),

        //! Parameter for maximum value of capacity credit
        DEFINE_VARIABLE( SIMPLE, "capacity-credit-max", mCapacityCreditMax, double ),

        //! Parameter for minimum value of capacity credit
        DEFINE_VARIABLE( SIMPLE, "capacity-credit-min", mCapacityCreditMin, double ),

        //! Parameter for steepness of capacity credit curve. 
        DEFINE_VARIABLE( SIMPLE, "steepness", mSteepness, double ),
       
        //! Parameter for half-life i.e. the value of the share at which mCapacityCredit = 0.5*(mCapacityCreditMax - mCapacityCreditMin)
        DEFINE_VARIABLE( SIMPLE, "x-mid", mXmid, double )
    )
};

#endif // _CAPACITY_CREDIT_CALCULATOR_H_
