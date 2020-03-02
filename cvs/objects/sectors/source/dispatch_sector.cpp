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
 * \file dispatch_sector.cpp
 * \ingroup Objects
 * \brief DispatchSector class source file.
 * \author Pralit Patel
 */

#include "util/base/include/definitions.h"
#include <string>
#include <algorithm>
#include <cassert>

// xml headers
#include "sectors/include/dispatch_sector.h"
#include "sectors/include/subsector.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/model_time.h"
#include "containers/include/market_dependency_finder.h"
#include "containers/include/iinfo.h"
#include "util/logger/include/ilogger.h"
#include "util/base/include/gcam_fusion.hpp"
#include "util/base/include/gcam_data_containers.h"
#include "sectors/include/sector_utils.h" // delete

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

/* \brief Constructor
 * \param aRegionName The name of the region.
 */
DispatchSector::DispatchSector( const string& aRegionName ):
SupplySector( aRegionName )
{
}

const string& DispatchSector::getXMLNameStatic() {
    const static string XML_NAME = "dispatch-sector";
    return XML_NAME;
}

bool DispatchSector::XMLDerivedClassParse( const string& aNodeName, const DOMNode* aNode ) {
    bool didParse = true;
    if( aNodeName == "cal-production" ) {
        XMLHelper<double>::insertValueIntoVector( aNode, mCalProduction, scenario->getModeltime() );
    }
    else if( aNodeName == "generation-sector" ) {
        mGenSectors.push_back(make_pair( XMLHelper<string>::getAttr( aNode, "name" ), XMLHelper<string>::getAttr( aNode, "market" ) ) );
    }
    else if( aNodeName == "demand-segment" ) {
        DemandSegment* currSeg = new DemandSegment;
        currSeg->mName = XMLHelper<string>::getAttr( aNode, "name" );
        currSeg->mHours = XMLHelper<double>::getAttr( aNode, "hours" );
        currSeg->mRelativeGen = XMLHelper<double>::getAttr( aNode, "relative-generation" );
        currSeg->mTotalGenFraction = XMLHelper<double>::getAttr( aNode, "generation-fraction" );
        mDemandSegments.push_back( currSeg );
    }
    else {
        didParse = false;
    }
    return didParse;
}

void DispatchSector::toInputXMLDerived( ostream& aOut, Tabs* aTabs ) const {
    //XMLWriteElement( mMarginalRevenueSector, "marginal-revenue-sector", aOut, aTabs );
    //XMLWriteElementCheckDefault( mMarginalRevenueMarket, "marginal-revenue-market", aOut, aTabs, mRegionName );
    for( auto genSector : mGenSectors ) {
        XMLWriteElement( genSector.second, "generation-sector", aOut, aTabs, 0, genSector.first );
    }
}

void DispatchSector::toDebugXMLDerived( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    toInputXMLDerived( aOut, aTabs );
    XMLWriteElement( mNewCapacity, "new-capacity", aOut, aTabs );
    XMLWriteElement( mExistingCapacity[ aPeriod ], "existing-capacity", aOut, aTabs );
    XMLWriteElement( mRequiredCapacity[ aPeriod ], "required-capacity", aOut, aTabs );
    for(auto techSegIt : mSaveTechCurve[ aPeriod ] ) {
        map<string, string> attrs;
        attrs["segment"] = get<1>(techSegIt.first);
        attrs["tech-name"] = get<0>(techSegIt.first)->getName();
        attrs["tech-year"] = util::toString(get<0>(techSegIt.first)->getYear());
        attrs["state"] = get<2>(techSegIt.first);
        XMLWriteElementWithAttributes( techSegIt.second, "segment-production", aOut, aTabs, attrs );
    }
}

void DispatchSector::completeInit( const IInfo* aRegionInfo,
                                      ILandAllocator* aLandAllocator )
{
    // always force trial markets
    //mUseTrialMarkets = mSubsectors.empty();
    SupplySector::completeInit( aRegionInfo, aLandAllocator );
    
    MarketDependencyFinder* depFinder = scenario->getMarketplace()->getDependencyFinder();
    for( auto genSector : mGenSectors ) {
        depFinder->addDependency( genSector.first, genSector.second, mName, mRegionName, false );
        if(mSubsectors.empty()) {
        depFinder->copyDependencies( genSector.first, genSector.second, mName, mRegionName );
        }
    }
    if(mSubsectors.empty()) {
    //depFinder->addDependency( mName, mRegionName, "capacity investment", mRegionName );
        Marketplace* marketplace = scenario->getMarketplace();
        for(auto segment : mDemandSegments) {
            const string segmentMarketName = mName+"_"+segment->mName;
            marketplace->createMarket( mRegionName, mRegionName, segmentMarketName, IMarketType::NORMAL);
            IInfo* marketInfo = marketplace->getMarketInfo( segmentMarketName, mRegionName, 0, true );
            marketInfo->setString( "price-unit", mPriceUnit );
            marketInfo->setString( "output-unit", mOutputUnit );

            depFinder->addDependency( segmentMarketName, mRegionName, segmentMarketName, mRegionName );
            depFinder->resolveActivityToDependency( mRegionName, segmentMarketName,
                                                   new DummyActivity(), new DummyActivity() );
            depFinder->copyDependencies( mName, mRegionName, segmentMarketName, mRegionName );
            depFinder->addDependency( segmentMarketName, mRegionName, mName, mRegionName );
        }
    }
}

class StringEqualsRef : public AMatchesValue {
public:
    StringEqualsRef( const std::string& aStr ):mStr( aStr ) {}
    virtual ~StringEqualsRef() {}
    virtual bool matchesString( const std::string& aStrToTest ) const {
        return mStr == aStrToTest;
    }
private:
    const std::string& mStr;
};

void DispatchSector::initCalc( NationalAccount* aNationalAccount,
                                  const Demographic* aDemographics,
                                  const int aPeriod )
{
    SupplySector::initCalc( aNationalAccount, aDemographics, aPeriod );
    
    if( mSubsectors.empty() ) {
        mDoGatherCapacity = false;
        mDoDispatchCapacity = true;
    }
    else {
        mDoGatherCapacity = true;
        mDoDispatchCapacity = false;
    }
    
    mAllTechs.clear();
    GetOpertingTechs getTechs;
    getTechs.mParent = this;
    getTechs.mPeriod = aPeriod;
    if( mDoGatherCapacity ) {
        getTechs.mGenSectorName = &mName;
        getTechs.mRegionName = &mRegionName;
        GCAMFusion<GetOpertingTechs> gatherTechs( getTechs, parseFilterString( "subsector/technology" ) );
        gatherTechs.startFilter( this );
    }
    else {
        string currRegion;
        string currSector;
        getTechs.mGenSectorName = &currSector;
        getTechs.mRegionName = &currRegion;
        vector<FilterStep*> getCapSteps;
        getCapSteps.push_back( new FilterStep( "world" ) );
        getCapSteps.push_back( new FilterStep( "region", new NamedFilter( new StringEqualsRef( currRegion ) ) ) );
        getCapSteps.push_back( new FilterStep( "sector", new NamedFilter( new StringEqualsRef( currSector ) ) ) );
        getCapSteps.push_back( new FilterStep( "subsector" ) );
        getCapSteps.push_back( new FilterStep( "technology" ) );
        GCAMFusion<GetOpertingTechs> gatherTechs( getTechs, getCapSteps );
        for( auto genSector : mGenSectors ) {
            currRegion = genSector.second;
            currSector = genSector.first;
            gatherTechs.startFilter( scenario );
        }
        for( auto filterStep : getCapSteps ) {
            delete filterStep;
        }
    }
    
    //std::cout << "Found techs: " << mAllTechs.size() << endl;
}

void DispatchSector::supply( const GDP* aGDP, const int aPeriod ) {
    Marketplace* marketplace = scenario->getMarketplace();
    // demand for the good produced by this Sector
    //double marketDemand = max( marketplace->getDemand( "electricity", mRegionName, aPeriod ), 0.0 );
    
    if( mDoGatherCapacity && aPeriod > scenario->getModeltime()->getFinalCalibrationPeriod() ) {
        int year = scenario->getModeltime()->getper_to_yr( aPeriod );
        string currRegion;
        string currSector;
        string currTech;
        vector<FilterStep*> getCapSteps;
        getCapSteps.push_back( new FilterStep( "world" ) );
        getCapSteps.push_back( new FilterStep( "region", new NamedFilter( new StringEqualsRef( currRegion ) ) ) );
        getCapSteps.push_back( new FilterStep( "sector", new NamedFilter( new StringEqualsRef( currSector ) ) ) );
        getCapSteps.push_back( new FilterStep( "subsector" ) );
        getCapSteps.push_back( new FilterStep( "technology", new NamedFilter( new StringEqualsRef( currTech ) ) ) );
        getCapSteps.push_back( new FilterStep( "period", new YearFilter( new IntEquals( year ) ) ) );
        GetCapacityHelper getCapHelper;
        getCapHelper.mPeriod = aPeriod;
        GCAMFusion<GetCapacityHelper> doGetCap( getCapHelper, getCapSteps );
        for( auto tech : mAllTechs ) {
            getCapHelper.mTotalCapacity = 0;
            currTech = tech->getName();
            if( tech->isNewInvestment( aPeriod ) ) {
                for( auto genSector: mGenSectors ) {
                    currRegion = genSector.second;
                    currSector = genSector.first;
                    doGetCap.startFilter( scenario );
                }
                tech->production( mRegionName, mName, getCapHelper.mTotalCapacity, -2.0, 0, aPeriod );
            }
        }
        for( auto filterStep : getCapSteps ) {
            delete filterStep;
        }
    }
    

    if( mDoDispatchCapacity ) {
        double totalElecDemand = 0.0;
        //if( aPeriod <= scenario->getModeltime()->getFinalCalibrationPeriod() ) {
            for( auto segment : mDemandSegments ) {
                totalElecDemand += marketplace->getDemand( mName + "_" + segment->mName, mRegionName, aPeriod );
            }
        //}
        auto sortedTechs = mAllTechs;
        sort( sortedTechs.begin(), sortedTechs.end(), [&]( ITechnology* aLHS, ITechnology* aRHS ) -> bool {
            auto lhsMarket = mAllTechMarketMap[ aLHS ];
            auto rhsMarket = mAllTechMarketMap[ aRHS ];
            return aLHS->getEnergyCost( lhsMarket.second, lhsMarket.first, aPeriod ) < aRHS->getEnergyCost( rhsMarket.second, rhsMarket.first, aPeriod );
        });
        
        map<ITechnology*, double> techProd;
        for( auto tech : sortedTechs ) {
            techProd[ tech ] = 0.0;
        }
        const double HOURS_IN_YEAR = 8760.0;
        const double RESERVE_FRACTION = 1.15;
        int currYear = scenario->getModeltime()->getper_to_yr( aPeriod );
        double maxExistingCapacity = 0.0;
        double maxRequiredCapacity = 0.0;
        double capacityPrice = marketplace->getPrice( "capacity investment" , mRegionName, aPeriod );
        double avgCost = 0.0;
        for( auto segment : mDemandSegments ) {
            const string segmentMarketName = mName+"_"+segment->mName;
            double segmentDemand = marketplace->getDemand(segmentMarketName, mRegionName, aPeriod);
            double remainingProduction = segmentDemand;
            double segmentScaleFraction = segment->mHours / HOURS_IN_YEAR;
            if( segment->mRelativeGen == 1.0 ) {
                maxRequiredCapacity = ( remainingProduction / segmentScaleFraction ) * RESERVE_FRACTION;
            }
            if( aPeriod <= scenario->getModeltime()->getFinalCalibrationPeriod() ) {
                remainingProduction = totalElecDemand * 1.0 / mDemandSegments.size();
                segmentScaleFraction = 1.0 / mDemandSegments.size();
            }
            for( auto tech : sortedTechs) {
                auto techMarket = mAllTechMarketMap[ tech ];
                tuple<ITechnology*, string, string> currSave = make_tuple( tech, segment->mName, techMarket.second );
                double maxProduction = dynamic_cast<CapacityTechnology*>( tech )->tryDispatch( techMarket.second, techMarket.first, segment->mName, remainingProduction, segmentScaleFraction, aPeriod );
                double currProduction = std::min( remainingProduction, maxProduction );
                mSaveTechCurve[ aPeriod ][currSave] = currProduction;
                techProd[ tech ] += currProduction;
                //bool wasRemainng = remainingProduction > 0.0;
                remainingProduction -= currProduction;
                /*if( wasRemainng && remainingProduction == 0.0 ) {
                    avgCost += tech->getEnergyCost( mRegionName, mName, aPeriod ) * segment.mHours;
                }*/
                avgCost += tech->getEnergyCost( mRegionName, mName, aPeriod ) * currProduction;
                if( segment->mRelativeGen == 1.0 && tech->getYear() < currYear ) {
                    maxExistingCapacity += maxProduction / segmentScaleFraction;
                }
            }
            if( remainingProduction != 0.0 ) {
                //cout << "Remaining production in segment: " << remainingProduction << " in " << mRegionName << ", " << mName << ", " << segment.mName << endl;
                //avgCost += sortedTechs.back()->getEnergyCost( mRegionName, mName, aPeriod ) * segment.mHours;
            }
        }
        avgCost = avgCost / totalElecDemand + capacityPrice;
        for( auto segment : mDemandSegments ) {
            segment->getCost() = avgCost;
            const string segmentMarketName = mName+"_"+segment->mName;
            marketplace->setPrice( segmentMarketName, mRegionName, avgCost, aPeriod );
        }
        //double remainingProduction = marketDemand;
        for( auto currProd : techProd ) {
            auto tech = currProd.first;
            auto techMarket = mAllTechMarketMap[ tech ];
            tech->production( techMarket.second, techMarket.first, currProd.second, -1.0, aGDP, aPeriod );
            double currTechOutput = tech->getOutput( aPeriod );
            //avgCost += currTechOutput * tech->getEnergyCost( techMarket.second, techMarket.first, aPeriod );
            if( !util::isEqual(currTechOutput, currProd.second) ) {
                cout << tech->getName() << ", " << tech->getYear() << " -> from seg: " << currProd.second << " production: " << currTechOutput << endl;
            }
            //remainingProduction -= currTechOutput;
        }
        /*if( remainingProduction != 0.0 ) {
            //cout << "Remaining production: " << remainingProduction << " in " << mRegionName << ", " << mName << endl;
        }*/
        mExistingCapacity = maxExistingCapacity;
        mRequiredCapacity = maxRequiredCapacity;

        /*avgCost = ( marketDemand == 0.0 ? avgCost : avgCost / marketDemand )
            + marketplace->getPrice( "capacity investment" , mRegionName, aPeriod );
        //cout << mRegionName << " Avg cost: " << avgCost << endl;
        mSupply = marketDemand;// - remainingProduction;
        marketplace->setPrice( mName, mRegionName, avgCost, aPeriod );
        marketplace->addToSupply( mName, mRegionName, mSupply, aPeriod );*/
        mNewCapacity = aPeriod > scenario->getModeltime()->getFinalCalibrationPeriod() ? std::max( maxRequiredCapacity - maxExistingCapacity , 0.0 ) : 0.0;
        marketplace->addToDemand( "capacity investment", mRegionName, mNewCapacity, aPeriod );
    }
}

void DispatchSector::postCalc( const int aPeriod ) {
    SupplySector::postCalc( aPeriod );
}

template<typename DataType>
void DispatchSector::GetOpertingTechs::processData( DataType& aData ) {
    //cout << "Found an unexpected state var type: " << typeid( aData ).name() << endl;
}

template<>
void DispatchSector::GetOpertingTechs::processData<ITechnologyContainer*>( ITechnologyContainer*& aData ) {
    for( auto iter = aData->getVintageBegin( mPeriod ); iter != aData->getVintageEnd( mPeriod); ++iter ) {
        if( (*iter).second->isOperating( mPeriod ) /*&& ( (*iter).second->isFixedOutputTechnology( mPeriod ) || (*iter).second->getShareWeight() > 0.0 )*/ ) {
            mParent->mAllTechs.push_back( (*iter).second );
            mParent->mAllTechMarketMap[ (*iter).second ] = make_pair( *mGenSectorName, *mRegionName );
        }
    }
}

template<typename DataType>
void DispatchSector::GetCapacityHelper::processData( DataType& aData ) {
    //cout << "Found an unexpected state var type: " << typeid( aData ).name() << endl;
}

template<>
void DispatchSector::GetCapacityHelper::processData<ITechnology*>( ITechnology*& aData ) {
    mTotalCapacity += aData->getOutput( mPeriod ) / aData->getCapacityFactor();
}
