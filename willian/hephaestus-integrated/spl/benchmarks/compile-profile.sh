#ghc -prof -auto -i.:../src/:../../feature-modeling/src/:../../util/src/:../../asset-base/req-model/src/:../../asset-base/uc-model/src/:../../asset-base/component-model/src/ --make Main.hs

ghc  -i.:../src/:../../feature-modeling/src/:../../util/src/:../../asset-base/req-model/src/:../../asset-base/uc-model/src/:../../asset-base/component-model/src/:../../asset-base/bpmn/src/:../../asset-base/ensemble/src/ --make Main.hs
