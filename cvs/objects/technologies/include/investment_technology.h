#ifndef _INVESTMENT_TECHNOLOGY_H_
#define _INVESTMENT_TECHNOLOGY_H_
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
* \file investment_technology.h
* \ingroup Objects
* \brief The InvestmentTechnology class header file.
* \details TODO:
*
* \author Gokul Iyer, Pralit Patel
*/

#include <xercesc/dom/DOMNode.hpp>
#include "technologies/include/technology.h"

// Forward declaration
class Tabs;
class CapacityCreditCalculator;

class InvestmentTechnology : public Technology {
    friend class XMLDBOutputter;
public:
    InvestmentTechnology(const std::string& aName,
        const int aYear);
    virtual ~InvestmentTechnology();
    static const std::string& getXMLNameStatic();
    InvestmentTechnology* clone() const;

    /*virtual void completeInit(const std::string& aRegionName,
        const std::string& aSectorName,
        const std::string& aSubsectorName,
        const IInfo* aSubsectorIInfo,
        ILandAllocator* aLandAllocator);

    virtual void initCalc(const std::string& aRegionName,
        const std::string& aSectorName,
        const IInfo* aSubsectorInfo,
        const Demographic* aDemographics,
        PreviousPeriodInfo& aPrevPeriodInfo,
        const int aPeriod);*/

    virtual void production(const std::string& aRegionName,
        const std::string& aSectorName,
        double aVariableDemand,
        double aFixedOutputScaleFactor,
        const GDP* aGDP,
        const int aPeriod);
    
    /*virtual void calcCost(const std::string& aRegionName,
        const std::string& aSectorName,
        const int aPeriod);

    virtual void doInterpolations(const Technology* aPrevTech, const Technology* aNextTech);*/

protected:
    
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        Technology,
    )
    
    //! pointer to the capacity credit input
    std::vector<IInput*>::iterator mCapacityCreditInput;

    virtual void toDebugXMLDerived(const int period, std::ostream& out, Tabs* tabs) const { }
    virtual bool XMLDerivedClassParse(const std::string& nodeName, const xercesc::DOMNode* curr) { return false; }
    virtual const std::string& getXMLName() const;
    //void copy( const InvestmentTechnology& aOther );
    //virtual void acceptDerived( IVisitor* aVisitor, const int aPeriod ) const;
};

class IntermittentInvestmentTechnology : public InvestmentTechnology {
    friend class XMLDBOutputter;
public:
    IntermittentInvestmentTechnology(const std::string& aName,
        const int aYear);
    virtual ~IntermittentInvestmentTechnology();
    static const std::string& getXMLNameStatic();
    IntermittentInvestmentTechnology* clone() const;

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
    
    virtual void calcCost(const std::string& aRegionName,
        const std::string& aSectorName,
        const int aPeriod);
    
    virtual double getCapacityFactor() const;

    virtual void doInterpolations(const Technology* aPrevTech, const Technology* aNextTech);

protected:
    
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        InvestmentTechnology,
        //! A calculator which determines the capacity credit as a function of renewable share.
        DEFINE_VARIABLE(CONTAINER, "capacity-credit-calculator", mCapacityCreditCalculator, CapacityCreditCalculator*),

        //! Name of trial market associated with this Investment Technology. This is read in only for intermittent-technologies
        //! for which trial market calculations are performed in the CapacityTechnology class. The value read in here should be
        //! equal to the value read in under the capacity technologies.
        DEFINE_VARIABLE(SIMPLE, "trial-market-name", mTrialMarketName, std::string),
                            
        //! Because intermittent investment technologies will essentially have a dynamic capacity factor we need to
        //! double check it exceeds the number of hours in this investment segment
        DEFINE_VARIABLE(SIMPLE, "max-capacity-factor", mMaxCapFac, double)
    )
    
    //! pointer to the capacity credit input
    std::vector<IInput*>::iterator mCapacityCreditInput;
    
    //! pointer to the resource input
    std::vector<IInput*>::iterator mResourceInput;

    virtual void toDebugXMLDerived(const int period, std::ostream& out, Tabs* tabs) const;
    virtual bool XMLDerivedClassParse(const std::string& nodeName, const xercesc::DOMNode* curr);
    virtual const std::string& getXMLName() const;
    void copy( const IntermittentInvestmentTechnology& aOther );
    virtual void acceptDerived( IVisitor* aVisitor, const int aPeriod ) const;
};

#endif // _INVESTMENT_TECHNOLOGY_H_

