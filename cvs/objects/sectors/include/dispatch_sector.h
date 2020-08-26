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

class CapacityTechnology;
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

    virtual void toDebugXMLDerived( const int period, std::ostream& aOut, Tabs* aTabs ) const;
    
    virtual void setMarket();
    
    double calcAggregateCapacity( const int aPeriod ) const;
    
    /*!
     * \brief A helper class to identify the demand side segment market names.
     * \todo We don't really need this as a class anymore
     */
    class DemandSegment : public INamed {
    protected:
        DEFINE_DATA(
            DEFINE_SUBCLASS_FAMILY( DemandSegment )
        )
        
    public:
        DemandSegment( const std::string& aName ):mName( aName ) {}
        virtual const std::string& getName() const {
            return mName;
        }
        std::string mName;
    };
    
    /*!
     * \brief A class to help organize all the dispatch segment parameters together.
     * \details Ideally this would be a struct but we need to make it a GCAM fusion
     *          aware class as the new investment quantity needs to be declared STATE.
     */
    class DispatchSegment : public INamed {
    protected:
        DEFINE_DATA(
            DEFINE_SUBCLASS_FAMILY( DispatchSegment ),
            
            //! Helper to be able to add the quantity of new investment into the market
            DEFINE_VARIABLE( SIMPLE | STATE, "new-investment", mNewCapacity, Value)
        )
    public:
        //! getName accessor as expected by GCAM fusion
        virtual const std::string& getName() const {
            return mName;
        }
        //! accessor to the protected new capacity member variables
        Value& getNewInvestment() {
            return mNewCapacity;
        }
        //! The name of this dispatch segment
        std::string mName;
        //! Mapping to the demand segment that this dispatch segment falls under
        std::string mDemandSegmentName;
        //! If not empty this dispatch sector is a marker for calculating new investment
        //! in the investment sector named here.
        std::string mInvestmentSegmentName;
        //! The number of hours in this segment
        double mHours;
        //! The relative relative load of this segment to superpeak
        double mRelativeGen;
        //! The fraction of energy that makes this dispatch segment fills of the
        //! total energy in mDemandSegmentName
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

        //! A list of the demand segment market names from which the consumers of
        //! electricity will log their demands
        DEFINE_VARIABLE( CONTAINER, "demand-segment", mDemandSegments, std::vector<DemandSegment*> ),
           
        //! The dispatch segments which will map onto the demand segments to generate
        //! the demanded electricity
        DEFINE_VARIABLE( CONTAINER, "dispatch-segment", mDispatchSegments, std::vector<DispatchSegment*> )
    )
    
    //! The set of active technologies which will be dispatched
    std::vector<CapacityTechnology*> mAllTechs;
    
    //! A mapping of technologies to the markets in which they actually live in case
    //! this dispatch sector is pulling technologies across states for instance.
    std::map<CapacityTechnology*, std::pair<std::string, std::string> > mAllTechMarketMap;
    
    //! A flag to help identify if this instance of DispatchSector only needs to gather
    //! new capacity.
    bool mDoGatherCapacity;
    
    //! A flag to help identify if this instance of DispatchSector only needs to calculate
    //! the dispatch.
    bool mDoDispatchCapacity;

    //! Save detailed technology dispatch information for reporting
    objects::PeriodVector<std::map<std::tuple<CapacityTechnology*, std::string, std::string>, double> > mSaveTechCurve;

private:
    void setFixedDemandsToMarket( const int aPeriod ) const;
    
    /*!
     * \brief A GCAMFusion helper to collect technologies from the given region and
     *        sector.
     */
    struct GetOpertingTechs {
        DispatchSector* mParent;
        std::string* mRegionName;
        std::string* mGenSectorName;
        int mPeriod;
        template<typename DataType>
        void processData( DataType& aData );
    };
    
    /*!
     * \brief A GCAMFusion helper to collect new capacity across some investment
     *        sectors.
     */
    struct GetCapacityHelper {
        int mPeriod;
        
        double mTotalCapacity;
        void getTotalCapacity();
        
        template<typename DataType>
        void processData( DataType& aData );
    };
};

#endif // _DISPATCH_SECTOR_H_

