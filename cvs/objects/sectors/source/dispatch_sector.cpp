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
    else if( aNodeName == "dispatch-segment" ) {
        DispatchSegment* currSeg = new DispatchSegment;
        currSeg->mName = XMLHelper<string>::getAttr( aNode, "name" );
        currSeg->mDemandSegmentName = XMLHelper<string>::getAttr( aNode, "demand-segment-name" );
        currSeg->mInvestmentSegmentName = XMLHelper<string>::getAttr( aNode, "investment-segment-name" );
        currSeg->mHours = XMLHelper<double>::getAttr( aNode, "hours" );
        currSeg->mRelativeGen = XMLHelper<double>::getAttr( aNode, "relative-generation" );
        currSeg->mTotalGenFraction = XMLHelper<double>::getAttr( aNode, "generation-fraction" );
        mDispatchSegments.push_back( currSeg );
    }
    else {
        didParse = false;
    }
    return didParse;
}

void DispatchSector::toDebugXMLDerived( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
	XMLWriteElement(calcAggregateCapacity(aPeriod), "aggregate-capacity", aOut, aTabs);
    map<string, string> attrs;
    for(auto techSegIt : mSaveTechCurve[ aPeriod ] ) {
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
    SupplySector::completeInit( aRegionInfo, aLandAllocator );
    
    MarketDependencyFinder* depFinder = scenario->getMarketplace()->getDependencyFinder();
    if(mSubsectors.empty()) {
        // This instance of DispatchSector will be responsible for dispatching and operating
        // capacity.
        Marketplace* marketplace = scenario->getMarketplace();
        
        // We would like to sort the dispatch segments from the least load to the highest
        // load as this will be useful when calculating capacity investment so that we can
        // carry forward investment decisions from lower segments, such as baseload, and take
        // them into account when calculating for upper segments.
        sort( mDispatchSegments.begin(), mDispatchSegments.end(), [&]( DispatchSegment* aLHS, DispatchSegment* aRHS ) -> bool {
            return aLHS->mRelativeGen < aRHS->mRelativeGen;
        });
        
        set<string> demandSegmentNames;
        for(auto dispSegment : mDispatchSegments) {
            // Find the unique set of demand segment market names which can be found
            // by checking the values in the dispatch segments
            if(demandSegmentNames.find(dispSegment->mDemandSegmentName) == demandSegmentNames.end()) {
                DemandSegment* newDmdSegment = new DemandSegment(dispSegment->mDemandSegmentName);
                mDemandSegments.push_back(newDmdSegment);
                demandSegmentNames.insert(dispSegment->mDemandSegmentName);
            }
            
            // If this dispatch segment is a marker for an investment segment we
            // should include a dependency on that investment segment name and force
            // it to create trial markets as there is a circular dependency in that
            // we need to have already gathered new investment to do dispatch but
            // we need to dispatch to know how much new investment we will need.
            if(dispSegment->mInvestmentSegmentName != "") {
                depFinder->addDependency(dispSegment->mInvestmentSegmentName, mRegionName, dispSegment->mInvestmentSegmentName, mRegionName);
            }
        }
        
        // We need to create the markets for the demand segments as no other object
        // will have done that
        for(auto segment : mDemandSegments) {
            const string segmentMarketName = segment->mName;
            bool isNew = marketplace->createMarket( mRegionName, mRegionName, segmentMarketName, IMarketType::NORMAL);
            if( isNew ) {
                IInfo* marketInfo = marketplace->getMarketInfo( segmentMarketName, mRegionName, 0, true );
                marketInfo->setString( "price-unit", mPriceUnit );
                marketInfo->setString( "output-unit", mOutputUnit );

                // force trial markets as there will be circular dependencies due to
                // cogen
                depFinder->addDependency( segmentMarketName, mRegionName, segmentMarketName, mRegionName );
                // the market is just used to pass demands, there is no actual activity
                // behind it so just put a dummy activity as a place holder
                depFinder->resolveActivityToDependency( mRegionName, segmentMarketName,
                                                       new DummyActivity(), new DummyActivity() );
                // finally associate this dispatch sector as a dependent on the demand segment market
                depFinder->addDependency( segmentMarketName, mRegionName, mName, mRegionName );
            }
        }
    }
    else {
        // This instance of DispatchSector is responsible for gathering capacity so we
        // will need to add dependencies on the investment sectors so they can distribute
        // new investments to the technologies before this class gathers the capacity
        for( auto genSector : mGenSectors ) {
            depFinder->addDependency( genSector.first, genSector.second, mName, mRegionName, false );
        }
    }
}

void DispatchSector::setMarket() {
    if(!mSubsectors.empty()) {
        // let the grid region create the market
        return;
    }
    Marketplace* marketplace = scenario->getMarketplace();
    
    
    if ( marketplace->createMarket( mRegionName, mRegionName, mName, IMarketType::NORMAL ) ) {
        // Set price and output units for period 0 market info
        IInfo* marketInfo = marketplace->getMarketInfo( mName, mRegionName, 0, true );
        marketInfo->setString( "price-unit", mPriceUnit );
        marketInfo->setString( "output-unit", mOutputUnit );
        
        // Set market prices to initial price vector
        marketplace->setPriceVector( mName, mRegionName, mPrice );
        
        // add states to market
        for( auto genSector : mGenSectors ) {
            marketplace->createMarket( genSector.second, mRegionName, mName, IMarketType::NORMAL );
        }
    }
}

/*!
 * \brief A predicate for GCAMFusion queries that just checks string equality
 *        but it compares it against a reference.  We do this so we can create
 *        the query one time but execute it over a list of strings to check.
 */
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
    
    // TODO: we are currently using the fact that the subector is empty to determine
    // if this instance of dispatch sector is going to calculate the dispatch or
    // just gather capacity.  This works for GCAM-USA but when we use it in the
    // global model it may not be correct.
    if( mSubsectors.empty() ) {
        mDoGatherCapacity = false;
        mDoDispatchCapacity = true;
    }
    else {
        mDoGatherCapacity = true;
        mDoDispatchCapacity = false;
    }
    
    mAllTechs.clear();
    mAllTechMarketMap.clear();
    GetOpertingTechs getTechs;
    getTechs.mParent = this;
    getTechs.mPeriod = aPeriod;
    vector<FilterStep*> getCapSteps;
    if( mDoGatherCapacity ) {
        // If we are just gather techs we will just get the instances of the new vintage
        // capacity technologies in this sector which we will update with the capacity
        // investment during supply
        getTechs.mGenSectorName = &mName;
        getTechs.mRegionName = &mRegionName;
        getCapSteps = parseFilterString( "subsector/technology" );
        GCAMFusion<GetOpertingTechs> gatherTechs( getTechs, getCapSteps );
        gatherTechs.startFilter( this );
    }
    else {
        // if this instance is doing dispatch we need to get a reference to the capacity
        // technologies to dispatch which may live in some separate region / sector
        string currRegion;
        string currSector;
        getTechs.mGenSectorName = &currSector;
        getTechs.mRegionName = &currRegion;
        getCapSteps.push_back( new FilterStep( "world" ) );
        // note the StringEqualsRef
        // this is so we can create the query one and re-use it as we loop over
        // region / sectors from which to gather technologies
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
    }
    // clean up memory from the GCAMFusion query
    for( auto filterStep : getCapSteps ) {
        delete filterStep;
    }
}

void DispatchSector::supply( const GDP* aGDP, const int aPeriod ) {
    Marketplace* marketplace = scenario->getMarketplace();
    
    if( mDoGatherCapacity && aPeriod > scenario->getModeltime()->getFinalCalibrationPeriod() ) {
        // this instance needs to gather new capacity investments
        // we use a GCAMFusion to query the technologies in the investment sectors
        // and collect new capacity investments by technology
        int year = scenario->getModeltime()->getper_to_yr( aPeriod );
        string currRegion;
        string currSector;
        string currTech;
        vector<FilterStep*> getCapSteps;
        getCapSteps.push_back( new FilterStep( "world" ) );
        // note the StringEqualsRef
        // this is so we can create the query one and re-use it as we loop over
        // region / sectors from which to gather capacity
        getCapSteps.push_back( new FilterStep( "region", new NamedFilter( new StringEqualsRef( currRegion ) ) ) );
        getCapSteps.push_back( new FilterStep( "sector", new NamedFilter( new StringEqualsRef( currSector ) ) ) );
        getCapSteps.push_back( new FilterStep( "subsector" ) );
        getCapSteps.push_back( new FilterStep( "technology", new NamedFilter( new StringEqualsRef( currTech ) ) ) );
        getCapSteps.push_back( new FilterStep( "period", new YearFilter( new IntEquals( year ) ) ) );
        GetCapacityHelper getCapHelper;
        getCapHelper.mPeriod = aPeriod;
        GCAMFusion<GetCapacityHelper> doGetCap( getCapHelper, getCapSteps );
        // gather capacity by technology
        for( auto tech : mAllTechs ) {
            getCapHelper.mTotalCapacity = 0;
            currTech = tech->getName();
            if( tech->isNewInvestment( aPeriod ) ) {
                // and collapse accross investment segment
                for( auto genSector: mGenSectors ) {
                    currRegion = genSector.second;
                    currSector = genSector.first;
                    // this will run the query and store the total new investment
                    // for this technology in getCapHelper.mTotalCapacity
                    doGetCap.startFilter( scenario );
                }
                // set the new investment capacity
                tech->setCapacity( getCapHelper.mTotalCapacity, aPeriod );
            }
        }
        // clean up memory from the GCAMFusion query
        for( auto filterStep : getCapSteps ) {
            delete filterStep;
        }

	
    }
    

    if( mDoDispatchCapacity ) {
		// this instance will actually dispatch the capacity technologies
        
        // calculate the total electricity demand which will be needed during calibration
        // or to calculate the total average average cost of electricity
        double totalElecDemand = 0.0;
        for( auto segment : mDemandSegments ) {
            // guard against negative demands
            // TODO: use 0.0 or util::getVerySmallNumber?  The later was put in as a test
            // to help with solving but is it really necessary?
            totalElecDemand += std::max(marketplace->getDemand( segment->mName, mRegionName, aPeriod ), util::getVerySmallNumber());
        }
        
        // sort the technologies by energy cost
        // we will also cache the costs so that we do not need to re-calculate them
        // over and over again
        auto sortedTechs = mAllTechs;
        map<CapacityTechnology*, double> techEnergyCostCache;
        map<CapacityTechnology*, double> techProd;
        for( auto tech : mAllTechs ) {
            auto techMarket = mAllTechMarketMap[ tech ];
            // Calcualte the "energy" costs which is really just subtracting off
            // the capital and OM_Fixed.  Note, the CapacityTechnology also tacks
            // on GHG costs as well.
            // save to avoid recalculating the costs again
            techEnergyCostCache[ tech ] = tech->getEnergyCost( techMarket.second, techMarket.first, aPeriod );
            // initialize production by tech to zero
            techProd[ tech ] = 0.0;
        }
        // sort technologies from lowest "energy" cost to highest so that we can
        // dispatch in that order
        sort( sortedTechs.begin(), sortedTechs.end(), [&]( CapacityTechnology* aLHS, CapacityTechnology* aRHS ) -> bool {
            return techEnergyCostCache[ aLHS ] < techEnergyCostCache[ aRHS ];
        });
        
        const double HOURS_IN_YEAR = 8760.0;
        const double RESERVE_FRACTION = 1.15;
        int currYear = scenario->getModeltime()->getper_to_yr( aPeriod );
        double remainingHours = HOURS_IN_YEAR;
        double totalNewInvest = 0.0;
        double capacityPrice = 2.5;//marketplace->getPrice( "capacity investment" , mRegionName, aPeriod );
        double avgCost = 0.0;
        
        // loop over each demand segment and figure out the how much electricity
        // was demanded
        for( auto demandSegment : mDemandSegments ) {
            const string segmentMarketName = demandSegment->mName;
            // TODO: use 0.0 or util::getVerySmallNumber?  The later was put in as a test
            // to help with solving but is it really necessary?
            double segmentDemand = std::max(marketplace->getDemand(segmentMarketName, mRegionName, aPeriod), util::getVerySmallNumber());
            // loop over each dispatch segment in this demand segment and dispatch
            // the technologies for each one
            for( auto segment : mDispatchSegments ) {
                if(segment->mDemandSegmentName == segmentMarketName) {
                    // initialize the amount of energy we need to dispatch to the total
                    // times the fraction which is provided by this dispatch segment
                    double remainingProduction = segmentDemand * segment->mTotalGenFraction;
                    // we can use this segmentScaleFraction to convert to load which
                    // we will need asking the technologies capacity can they provide
                    // to supply this energy
                    double segmentScaleFraction = segment->mHours / HOURS_IN_YEAR;
                    
                    // NOTE: we do not actually have calibration information about technology
                    // dispatch order.  Instead we simply rescale ALL dispatch segments to be
                    // equal and the technologies will be using a calibrated total capacity
                    // factor.  Thus in *total* we will be able to match historical values
                    if( aPeriod <= scenario->getModeltime()->getFinalCalibrationPeriod() ) {
                        remainingProduction = totalElecDemand * 1.0 / mDemandSegments.size();
                        segmentScaleFraction = 1.0 / mDemandSegments.size();
                    }
                    
                    // If this dispatch segment was tagged as a marker segment for
                    // an investment segment we will need to calculate how much new
                    // capacity we want to invest in which is dependent on the price
                    // of investing in brand new capacity in that investment segment.
                    bool isInvestSegment = !segment->mInvestmentSegmentName.empty();
                    double investNewCost = isInvestSegment ? marketplace->getPrice(segment->mInvestmentSegmentName, mRegionName, aPeriod) : 0.0;
                    double currMaxInvestCap = ( remainingProduction / segmentScaleFraction ) * RESERVE_FRACTION;
                    double currExistCap = 0.0;

                    // do the dispatch
                    for( auto tech : sortedTechs) {
                        auto techMarket = mAllTechMarketMap[ tech ];
                        // so we can save dispatch by region + dispatch sector + technology
                        tuple<CapacityTechnology*, string, string> currSave = make_tuple( tech, segment->mName, techMarket.second );
                        
                        // Ask the technology technology how much energy it _could_ provide
                        // to fill the remaining demand.  This will depend on how much capacity
                        // there is, it's capacity factor (which may vary depending on the segment
                        // such as for wind and solar), as well as how hours are left to dispatch as
                        // some capacity may have a minimum capacity factor below which it can no
                        // operate at all.
                        double maxProduction = tech->tryDispatch( techMarket.second,
                                                                  techMarket.first,
                                                                  segment->mName,
                                                                  segmentScaleFraction,
                                                                  remainingHours / HOURS_IN_YEAR,
                                                                  techProd[ tech ],
                                                                  aPeriod );
                        
                        // remove the generation from this tech from the remaining
                        // the actual generation may be different than the maxProduction
                        // if there was less remaining generation require
                        double currProduction = std::max(std::min( remainingProduction, maxProduction ), 0.0);
                        mSaveTechCurve[ aPeriod ][currSave] = currProduction;
                        techProd[ tech ] += currProduction;
                        remainingProduction -= currProduction;
                        // we are calculating the cost as the weighted average across all segments
                        avgCost += techEnergyCostCache[ tech ] * currProduction;

                        // calculate the amount of existing capacity available in this
                        // segment to determine how much new investment may need to be made
                        if( isInvestSegment && tech->getYear() < currYear ) {
                            currExistCap += tech->calcInvestmentCapacityScaleFactor( techMarket.second, techMarket.first, investNewCost, aPeriod )
                                * maxProduction / segmentScaleFraction;
                        }
                    }
                    
                    // TODO: some error checking to ensure there was enough capacity to meet demand
                    /*if( remainingProduction != 0.0 ) {
                        cout << "Remaining production in segment: " << remainingProduction << " in " << mRegionName << ", " << mName << ", " << segment.mName << endl;
                    }*/
                    
                    // now that we have looped all of the technologies we can update the
                    // market on how much new investment may be needed
                    if( isInvestSegment ) {
                        // TODO: right now it helps solution if we don't set an absolute zero
                        // for new investment to ensure the trial market doesn't get stuck as
                        // unsolvable
                        double currNewInvest = aPeriod > scenario->getModeltime()->getFinalCalibrationPeriod() ? std::max( currMaxInvestCap - currExistCap - totalNewInvest , 2 * util::getSmallNumber() ) : 0.0;
                        totalNewInvest += currNewInvest;
                        segment->getNewInvestment() = currNewInvest;
                        marketplace->addToDemand( segment->mInvestmentSegmentName, mRegionName, segment->getNewInvestment(), aPeriod );
                    }
                    remainingHours -= segment->mHours;

                }
            }
        }
        // calculate the average cost and add on the capacity investment cost
        avgCost = avgCost / totalElecDemand + capacityPrice;
        marketplace->setPrice( mName, mRegionName, avgCost, aPeriod );
        // right now we are assuming the same price in all demand segments
        // this is mostly because we can't calibrate it
        for( auto segment : mDemandSegments ) {
            const string segmentMarketName = segment->mName;
            marketplace->setPrice( segmentMarketName, mRegionName, avgCost, aPeriod );
        }
        
        // now that we have the total annual production for each technology we can
        // operate them using the normal equations to calculate input demands and
        // emissions
        for( auto currProd : techProd ) {
            auto tech = currProd.first;
            auto techMarket = mAllTechMarketMap[ tech ];
            tech->production( techMarket.second, techMarket.first, currProd.second, 1.0, aGDP, aPeriod );
        }
        
        // TODO: the following is assuming the market name for the capacity credits
        // is the grid region.  We could potentially make it more flexible with some
        // modest complication of this code.
        
        // Next we perform Trial Market Calculations for Capacity Credits.
        // Typically, we want all regions (states) within a containing grid region
        // to see the same capacity market price and capacity payments based on share
        // of renewables in that grid region. Hence, we begin by aggregating capacity
        // in the grid region (that is, when mDoDispatchCapacity = True).
        
        // Aggregating capacity across all capacity technologies for capacity credits
        // calculations to be done in CapacityTechnology and InvestmentTechnology classes.
        double aggregateCapacity = calcAggregateCapacity(aPeriod);
	
        // Calling the CapacityTechnology::addCapacityShareToMarket method which will
        // add the capacity shares of intermittent (i.e. non-dispatchable technologies)
        // to the trial market using aggregate capacity as calculated for the entire grid region.
        for (auto tech : mAllTechs) {
            tech->addCapacityShareToMarket(aggregateCapacity, mRegionName, mName, aPeriod);
        }
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
        if( (*iter).second->isOperating( mPeriod ) ) {
            CapacityTechnology* currTech = dynamic_cast<CapacityTechnology*>( (*iter).second );
            if( !currTech ) {
                ILogger& mainLog = ILogger::getLogger("main_log");
                mainLog.setLevel(ILogger::SEVERE);
                mainLog << mParent->getXMLName() << " " << mParent->mName
                    << " found technology that is not of type CapacityTechnology "
                    << " in region " << *mRegionName << " and sector " << *mGenSectorName
                    << ": " << (*iter).second->getName() << ", year: " << (*iter).second->getYear() << endl;
                abort();
            }
            mParent->mAllTechs.push_back( currTech );
            mParent->mAllTechMarketMap[ currTech ] = make_pair( *mGenSectorName, *mRegionName );
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


/*!
 * \brief The calcAggregateCapacity method aggregates capacity across all capacity-technology vintages.
 * \details We use mAllTechs member variable which contains all capacity-technologies by vintage from the dispatch sector
 *		    to loop through all capacity technology vintages. This method is called in the DispactchSector::Supply method to
 *          add capacity shares of intermittent (i.e. non-dispatchable) technologies to the trial market. This method
 *          calls the CapacityTechnology::getCapacity() method which accounts only for natural retirements.
 *          Note that we used "AggregateCapacity" since "TotalCapacity" corresponds to capacity
 *          of a single technology summed across investment segments.
 * \param aPeriod Model period.
 * \return aggregate capacity which is the total capacity of all capacity vintages within the containing sector. This includes natural
 *							retirements.
 */
double DispatchSector::calcAggregateCapacity(const int aPeriod) const {
    double aggregateCapacity = 0;
    for (auto tech : mAllTechs) {
        auto techMarketIter = mAllTechMarketMap.find( tech );
        assert( techMarketIter != mAllTechMarketMap.end() );
        auto techMarket = (*techMarketIter).second;
        double capacity = tech->getCapacity(techMarket.second, techMarket.first, aPeriod);
        aggregateCapacity += capacity;
    }
    return aggregateCapacity;
}

