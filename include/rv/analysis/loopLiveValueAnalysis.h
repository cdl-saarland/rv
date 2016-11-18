//===- loopLiveValueAnalysis.h ----------------*- C++ -*-===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @authors karrenberg
//

#ifndef _LOOPLIVEVALUEANALYSIS_H
#define _LOOPLIVEVALUEANALYSIS_H

#include <llvm/Pass.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/ADT/SetVector.h>
#include <llvm/ADT/DenseMap.h>

#include <rv/rvInfo.h>
#include <rv/vectorizationInfo.h>

namespace llvm {
class Value;
class BasicBlock;
class Loop;
class SelectInst;
}

namespace rv {
class VectorizationInfo;
}

using namespace llvm;
using namespace rv;

//namespace {

class LoopResults {
private:
    const Loop*                               mLoop;
    DenseMap<const Instruction*, PHINode*>    mLoopResultPhiMap;
    DenseMap<const Instruction*, SelectInst*> mLoopResultMap;
public:
    LoopResults(const Loop* l) : mLoop(l) {}
    ~LoopResults() {
        mLoopResultMap.clear();
        mLoopResultPhiMap.clear();
    }
    bool hasResult(const Instruction* inst) const {
        assert (inst);
        return mLoopResultMap.count(inst);
    }
    PHINode* getResultPhi(const Instruction* inst) const {
        assert (inst);
        DenseMap<const Instruction*, PHINode*>::const_iterator it = mLoopResultPhiMap.find(inst);
        if (it == mLoopResultPhiMap.end()) return NULL;
        return it->second;
    }
    SelectInst* getResult(const Instruction* inst) const {
        assert (inst);
        DenseMap<const Instruction*, SelectInst*>::const_iterator it = mLoopResultMap.find(inst);
        if (it == mLoopResultMap.end()) return NULL;
        return it->second;
    }
    void addResult(const Instruction* liveValue, PHINode* phi, SelectInst* update) {
        assert (liveValue && phi && update);
        assert (mLoop->contains(liveValue->getParent()));
        mLoopResultPhiMap.insert(std::make_pair(liveValue, phi));
        mLoopResultMap.insert(std::make_pair(liveValue, update));
    }
    void addResultPhi(const Instruction* liveValue, PHINode* phi) {
        assert (liveValue && phi);
        assert (mLoop->contains(liveValue->getParent()));
        mLoopResultPhiMap.insert(std::make_pair(liveValue, phi));
    }
    void addResultSelect(const Instruction* liveValue, SelectInst* update) {
        assert (liveValue && update);
        assert (mLoop->contains(liveValue->getParent()));
        mLoopResultMap.insert(std::make_pair(liveValue, update));
    }
    bool empty() const { return mLoopResultPhiMap.empty() && mLoopResultMap.empty(); }
    bool verify() const {
        const bool a = mLoop != NULL;
        const bool b = mLoopResultPhiMap.size() == mLoopResultMap.size();
        bool c = true;
        for (DenseMap<const Instruction*, PHINode*>::const_iterator it=mLoopResultPhiMap.begin(),
            E=mLoopResultPhiMap.end(); it!=E; ++it) {
            const Instruction* instA = it->first;
            c &= getResult(instA) != NULL;
        }
        bool d = true;
        for (DenseMap<const Instruction*, SelectInst*>::const_iterator it=mLoopResultMap.begin(),
            E=mLoopResultMap.end(); it!=E; ++it) {
            const Instruction* instA = it->first;
            d &= getResultPhi(instA) != NULL;
        }
        const bool res = a && b && c && d;
        assert(res && "could not verify LoopResult!");
        return res;
    }
};

class LoopLiveValueAnalysis;

class LoopLiveValueAnalysisWrapper : public FunctionPass
{
    LoopLiveValueAnalysis* mAnalysis;
public:
    static char ID __attribute__((unused)); // Pass identification, replacement for typeid.

    LoopLiveValueAnalysisWrapper();

    virtual void releaseMemory   ();
    virtual void getAnalysisUsage(AnalysisUsage &AU) const;
    virtual bool doInitialization(Module& M);
    virtual bool doFinalization  (Module& M);
    virtual bool runOnFunction   (Function& F);
    virtual void print           (raw_ostream& O, const Module* M) const;

    LoopLiveValueAnalysis* getLLVAnalysis()
    {
        return mAnalysis;
    }
};

class LoopLiveValueAnalysis
{
public:
    LoopLiveValueAnalysis(const rv::RVInfo&        rvInfo,
                          const LoopInfo&          loopInfo,
                          const VectorizationInfo& vecInfo);
	~LoopLiveValueAnalysis();

    bool run(Function& F);

    void dump(raw_ostream& O);

    class LoopLiveValueInfo
    {
    private:
        // For testing purposes, we want the live values to be ordered
        // deterministically (order of insertion).
        typedef SetVector<Instruction*> LiveValueVecType;
        LiveValueVecType mLiveValues;

    public:
        LoopLiveValueInfo();
        ~LoopLiveValueInfo();

        typedef LiveValueVecType::iterator       iterator;
        typedef LiveValueVecType::const_iterator const_iterator;
        iterator       begin()       { return mLiveValues.begin(); };
        const_iterator begin() const { return mLiveValues.begin(); };
        iterator       end()         { return mLiveValues.end(); };
        const_iterator end() const   { return mLiveValues.end(); };

        bool empty() const { return mLiveValues.empty(); }
        bool hasValue(Instruction* value) const { return mLiveValues.count(value); }

        void insert(Instruction* value);
        void remove(Instruction* value);
        void update(Instruction* value, Instruction* newValue);

        void printValues() const;
    };

    void printLoopLiveValues(const Loop* loop) const;
    LoopLiveValueInfo* getLiveValueInfo(const Loop* loop) const;
    void removeLiveValue(Instruction* value);
    void updateLiveValue(Instruction* value, Instruction* newValue);

    void setLoopResults(const Loop* loop, LoopResults* results);
    bool hasLoopResults(const Loop* loop) const;
    LoopResults* getLoopResults(const Loop* loop) const;

private:
    const rv::RVInfo&        mInfo;
    const LoopInfo&          mLoopInfo;
    const VectorizationInfo& mVecInfo;

    DenseMap<const Loop*, LoopLiveValueInfo*> mLiveValueMaps;
    DenseMap<const Loop*, LoopResults*>       mLoopResultMap;

    void findAllLoopLiveValues(Loop* loop);
    void findLoopLiveValues(Loop* loop, LoopLiveValueInfo& liveValueSet);
    Instruction* findUseOutsideLoop(Instruction* inst, const Loop* loop) const;
    PHINode* findLoopPhiForInstruction(Instruction* inst, Loop* loop) const;
    bool isPhiInductionVariable(const PHINode& phi, const Loop& loop) const;
};



// Forward declaration of initializer and public interface.
namespace llvm {
void initializeLoopLiveValueAnalysisWrapperPass(PassRegistry&);
FunctionPass* createLoopLiveValueAnalysisPass();
}


#endif /* _LOOPLIVEVALUEANALYSIS_H */
