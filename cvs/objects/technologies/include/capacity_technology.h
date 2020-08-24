#ifndef _CAPACITY_TECHNOLOGY_H_
#define _CAPACITY_TECHNOLOGY_H_
#if defined(_MSC_VER)
#pragma once
#endif

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
* \file capacity_technology.h
* \ingroup Objects
* \brief The CapacityTechnology class header file.
* \details TODO:
*
* \author Pralit Patel, Gokul Iyer
*/

#include <xercesc/dom/DOMNode.hpp>
#include "technologies/include/technology.h"

// Forward declaration
class Tabs;

class CapacityTechnology : public Technology {
	friend class XMLDBOutputter;
public:
	CapacityTechnology(const std::string& aName,
		const int aYear);
	~CapacityTechnology();
	static const std::string& getXMLNameStatic();
	CapacityTechnology* clone() const;

	virtual void completeInit(const std::string& aRegionName,
		const std::string& aSectorName,
		const std::string& aSubsectorName,
		const IInfo* aSubsectorIInfo,
		ILandAllocator* aLandAllocator);

	virtual void initCalc(const std::string& aRegionName,
		const std::string& aSectorName,
		const IInfo* aSubsectorInfo,
		const Demographic* aDemographics,
		PreviousPeriodInfo& aPrevPeriodInfo,
		const int aPeriod);
    
    virtual double getEnergyCost( const std::string& aRegionName,
                                  const std::string& aSectorName,
                                  const int aPeriod ) const;

	virtual void production(const std::string& aRegionName,
		const std::string& aSectorName,
		double aVariableDemand,
		double aFixedOutputScaleFactor,
		const GDP* aGDP,
		const int aPeriod);
    
    double tryDispatch( const std::string& aRegionName, const std::string& aSectorName,
                        const std::string& aDemandSegment,
                        const double aSegmentScaleFactor, const double aPercentRemainHours,
                        const double aPriorDispatch, const int aPeriod ) const;
    
    double calcInvestmentCapacityScaleFactor( const double aNewInvestCost, const int aPeriod ) const;

	virtual void doInterpolations(const Technology* aPrevTech, const Technology* aNextTech);

	virtual double getCapacity(	const std::string& aRegionName,
													const std::string& aSectorName, 
													const int aPeriod ) const;

	virtual void  addCapacityShareToMarket(double aAggregateCapacity,
		const std::string& aRegionName,
		const std::string& aSectorName,
		const int aPeriod);

protected:
    
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        Technology,

		//! Name of trial market associated with this Capacity Technology.
		DEFINE_VARIABLE(SIMPLE, "trial-market-name", mTrialMarketName, std::string),

		//! Name of capacity market associated with this Capacity Technology.
		DEFINE_VARIABLE(SIMPLE, "capacity-market-name", mCapacityMarketName, std::string),

        //! The capacity for this technology.
        DEFINE_VARIABLE( SIMPLE | STATE, "capacity", mCapacity, Value ),
                            
        DEFINE_VARIABLE( SIMPLE, "segment-capacity-factor", mSegCapFac, std::map<std::string, double> ),

		//! State value necessary to track tech output ration
		DEFINE_VARIABLE(SIMPLE | STATE, "tech-output-ratio", mIntermitOutTechRatio, Value),
   
        DEFINE_VARIABLE( SIMPLE , "min-capacity-factor", mMinCapFac, Value )
    )

	virtual void toInputXMLDerived(std::ostream& out, Tabs* tabs) const;
	virtual void toDebugXMLDerived(const int period, std::ostream& out, Tabs* tabs) const;
	virtual bool XMLDerivedClassParse(const std::string& nodeName, const xercesc::DOMNode* curr);
	//virtual void acceptDerived(IVisitor* aVisitor, const int aPeriod) const;
	virtual const std::string& getXMLName() const;
    void copy( const CapacityTechnology& aOther );
    virtual void setProductionState( const int aPeriod );
    virtual void acceptDerived( IVisitor* aVisitor, const int aPeriod ) const; 
};

#endif // _CAPACITY_TECHNOLOGY_H_

