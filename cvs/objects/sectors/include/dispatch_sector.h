#ifndef _DISPATH_SECTOR_H_
#define _DISPATH_SECTOR_H_
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
 * \file dispatch_sector.h
 * \ingroup Objects
 * \brief The DispatchSector class header file.
 * \author Pralit Patel
 */
#include <string>
#include <map>
#include <vector>
#include "sectors/include/supply_sector.h"
#include "containers/include/iactivity.h"

class ITechnology;
class FilterStep;

/*!
 * \ingroup Objects
 * \brief This class represents a dispatch supply sector.
 * \author Pralit Patel
 */
class DispatchSector: public SupplySector
{
    friend class XMLDBOutputter;
public:
    explicit DispatchSector( const std::string& aRegionName );
    virtual ~DispatchSector(){};
    static const std::string& getXMLNameStatic();

    virtual void completeInit( const IInfo* aRegionInfo,
                               ILandAllocator* aLandAllocator );


    virtual void initCalc( NationalAccount* aNationalAccount,
                           const Demographic* aDemographics,
                           const int aPeriod );
    
    virtual void supply( const GDP* aGDP,
                         const int aPeriod );
    
    virtual void postCalc( const int aPeriod );

protected:
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );

    virtual void toInputXMLDerived( std::ostream& aOut, Tabs* aTabs ) const;

    virtual void toDebugXMLDerived( const int period, std::ostream& aOut, Tabs* aTabs ) const;
    
    class DemandSegment : public INamed {
    protected:
        DEFINE_DATA(
                    DEFINE_SUBCLASS_FAMILY( DemandSegment ),
                    
                    /*DEFINE_VARIABLE( SIMPLE, "name", mName, std::string ),
                    DEFINE_VARIABLE( SIMPLE, "hours", mHours, double),
                    DEFINE_VARIABLE( SIMPLE, "relative-gen", mRelativeGen, double),
                    DEFINE_VARIABLE( SIMPLE, "total-gen-fraction", mTotalGenFraction, double),*/
                    DEFINE_VARIABLE( SIMPLE | STATE, "segment-cost", mCost, Value)
                    )
        
    public:
        DemandSegment( const std::string& aName ):mName( aName ) {}
        virtual const std::string& getName() const {
            return mName;
        }
        Value& getCost() {
            return mCost;
        }
        std::string mName;
    };
    
    class DispatchSegment : public INamed {
    public:
        virtual const std::string& getName() const {
            return mName;
        }
        std::string mName;
        std::string mDemandSegmentName;
         double mHours;
         double mRelativeGen;
         double mTotalGenFraction;
    };
    
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        SupplySector,
        
        //! The calibration capacity values
        DEFINE_VARIABLE( ARRAY, "cal-production", mCalProduction, objects::PeriodVector<double> ),
                            
        //! The load generation curve sectors mapped to percent energy coef
        DEFINE_VARIABLE( SIMPLE, "generation-sector", mGenSectors, std::vector<std::pair<std::string, std::string> > ),

        DEFINE_VARIABLE( CONTAINER, "demand-segment", mDemandSegments, std::vector<DemandSegment*> ),
                            
        DEFINE_VARIABLE( SIMPLE, "dispatch-segment", mDispatchSegments, std::vector<DispatchSegment*> ),
                            
        //! Helper to set supply = demand
        DEFINE_VARIABLE( SIMPLE | STATE, "supply-state", mSupply, Value ),
    
        //! New capacity to add
        DEFINE_VARIABLE( SIMPLE | STATE, "new-capacity", mNewCapacity, Value )
    )
    
    std::vector<ITechnology*> mAllTechs;
    
    std::map<ITechnology*, std::pair<std::string, std::string> > mAllTechMarketMap;
    
    bool mDoGatherCapacity;
    
    bool mDoDispatchCapacity;
    
    objects::PeriodVector<double> mExistingCapacity;
    objects::PeriodVector<double> mRequiredCapacity;
    objects::PeriodVector<std::map<std::tuple<ITechnology*, std::string, std::string>, double> > mSaveTechCurve;

private:
    void setFixedDemandsToMarket( const int aPeriod ) const;
    
    struct GetOpertingTechs {
        DispatchSector* mParent;
        std::string* mRegionName;
        std::string* mGenSectorName;
        int mPeriod;
        template<typename DataType>
        void processData( DataType& aData );
    };
    
    struct GetCapacityHelper {
        int mPeriod;
        
        double mTotalCapacity;
        void getTotalCapacity();
        
        template<typename DataType>
        void processData( DataType& aData );
    };
};

#endif // _DISPATCH_SECTOR_H_

