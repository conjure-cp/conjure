(set-logic QF_LRA)
(set-info :source | TTA Startup. Bruno Dutertre (bruno@csl.sri.com) |)
(set-info :smt-lib-version 2.0)
(set-info :category "industrial")
(set-info :status unsat)
(declare-fun x_0 () Bool)
(declare-fun x_1 () Bool)
(declare-fun x_2 () Bool)
(declare-fun x_3 () Real)
(declare-fun x_4 () Real)
(declare-fun x_5 () Real)
(declare-fun x_6 () Real)
(declare-fun x_7 () Real)
(declare-fun x_8 () Bool)
(declare-fun x_9 () Bool)
(declare-fun x_10 () Bool)
(declare-fun x_11 () Bool)
(declare-fun x_12 () Real)
(declare-fun x_13 () Real)
(declare-fun x_14 () Real)
(declare-fun x_15 () Bool)
(declare-fun x_16 () Bool)
(declare-fun x_17 () Bool)
(declare-fun x_18 () Bool)
(declare-fun x_19 () Bool)
(declare-fun x_20 () Bool)
(declare-fun x_21 () Real)
(declare-fun x_22 () Real)
(declare-fun x_23 () Real)
(declare-fun x_24 () Real)
(declare-fun x_25 () Real)
(declare-fun x_26 () Real)
(declare-fun x_27 () Real)
(declare-fun x_28 () Real)
(declare-fun x_29 () Real)
(declare-fun x_30 () Bool)
(declare-fun x_31 () Bool)
(declare-fun x_32 () Bool)
(declare-fun x_33 () Bool)
(declare-fun x_34 () Real)
(declare-fun x_35 () Real)
(declare-fun x_36 () Real)
(declare-fun x_37 () Bool)
(declare-fun x_38 () Bool)
(declare-fun x_39 () Bool)
(declare-fun x_40 () Bool)
(declare-fun x_41 () Bool)
(declare-fun x_42 () Bool)
(declare-fun x_43 () Real)
(declare-fun x_44 () Real)
(declare-fun x_45 () Real)
(declare-fun x_46 () Real)
(declare-fun x_47 () Real)
(declare-fun x_48 () Real)
(declare-fun x_49 () Real)
(declare-fun x_50 () Real)
(declare-fun x_51 () Real)
(declare-fun x_52 () Real)
(declare-fun x_53 () Real)
(declare-fun x_54 () Real)
(declare-fun x_55 () Real)
(declare-fun x_56 () Real)
(declare-fun x_57 () Real)
(declare-fun x_58 () Real)
(declare-fun x_59 () Real)
(declare-fun x_60 () Real)
(declare-fun x_61 () Real)
(declare-fun x_62 () Real)
(declare-fun x_63 () Real)
(declare-fun x_64 () Real)
(declare-fun x_65 () Real)
(declare-fun x_66 () Real)
(declare-fun x_67 () Real)
(declare-fun x_68 () Real)
(declare-fun x_69 () Real)
(declare-fun x_70 () Real)
(declare-fun x_71 () Real)
(declare-fun x_72 () Real)
(declare-fun x_73 () Real)
(declare-fun x_74 () Real)
(declare-fun x_75 () Real)
(assert (let ((?v_187 (not (< x_4 (+ x_12 24)))) (?v_189 (not (< x_5 (+ x_12 27)))) (?v_191 (not (< x_6 (+ x_12 30)))) (?v_193 (not (< x_7 (+ x_12 33)))) (?v_139 (= x_21 0)) (?v_140 (= x_21 1)) (?v_148 (= x_21 2)) (?v_141 (= x_22 0)) (?v_142 (= x_22 1)) (?v_150 (= x_22 2)) (?v_143 (= x_23 0)) (?v_144 (= x_23 1)) (?v_152 (= x_23 2)) (?v_145 (= x_24 0)) (?v_146 (= x_24 1)) (?v_154 (= x_24 2))) (let ((?v_171 (not ?v_140)) (?v_163 (not ?v_148)) (?v_172 (not ?v_142)) (?v_165 (not ?v_150)) (?v_174 (not ?v_144)) (?v_167 (not ?v_152)) (?v_176 (not ?v_146)) (?v_169 (not ?v_154)) (?v_197 (= x_23 3))) (let ((?v_202 (not ?v_197)) (?v_319 (= x_3 x_6))) (let ((?v_209 (not ?v_319)) (?v_198 (= x_24 3))) (let ((?v_203 (not ?v_198)) (?v_332 (= x_3 x_7))) (let ((?v_211 (not ?v_332)) (?v_173 (not (<= (- x_5 x_4) x_14))) (?v_175 (not (<= (- x_6 x_4) x_14))) (?v_177 (not (<= (- x_7 x_4) x_14))) (?v_178 (not (<= (- x_6 x_5) x_14))) (?v_179 (not (<= (- x_7 x_5) x_14))) (?v_180 (not (<= (- x_7 x_6) x_14))) (?v_277 (= x_47 0)) (?v_182 (not (< (- x_4 x_12) 12))) (?v_164 (<= (- x_4 x_3) 12)) (?v_183 (not (< (- x_5 x_12) 15))) (?v_166 (<= (- x_5 x_3) 15)) (?v_184 (not (< (- x_6 x_12) 18))) (?v_168 (<= (- x_6 x_3) 18)) (?v_185 (not (< (- x_7 x_12) 21))) (?v_170 (<= (- x_7 x_3) 21)) (?v_206 (= x_48 x_49)) (?v_208 (= x_48 x_50)) (?v_210 (= x_48 x_51)) (?v_213 (= x_49 x_48)) (?v_217 (= x_49 x_50)) (?v_220 (= x_49 x_51)) (?v_222 (= x_50 x_48)) (?v_223 (= x_50 x_49)) (?v_224 (= x_50 x_51)) (?v_225 (= x_51 x_48)) (?v_226 (= x_51 x_49)) (?v_227 (= x_51 x_50)) (?v_255 (< x_3 x_4))) (let ((?v_204 (not ?v_255)) (?v_240 (= x_48 x_53)) (?v_256 (< x_3 x_5))) (let ((?v_212 (not ?v_256)) (?v_242 (= x_49 x_53)) (?v_257 (< x_3 x_6))) (let ((?v_216 (not ?v_257)) (?v_244 (= x_50 x_53)) (?v_258 (< x_3 x_7))) (let ((?v_219 (not ?v_258)) (?v_246 (= x_51 x_53)) (?v_194 (= x_21 3))) (let ((?v_199 (not ?v_194)) (?v_265 (= x_3 x_4))) (let ((?v_205 (not ?v_265)) (?v_195 (= x_22 3))) (let ((?v_200 (not ?v_195)) (?v_303 (= x_3 x_5))) (let ((?v_207 (not ?v_303)) (?v_186 (and x_8 (< x_13 x_4))) (?v_188 (and x_9 (< x_13 x_5))) (?v_190 (and x_10 (< x_13 x_6))) (?v_192 (and x_11 (< x_13 x_7))) (?v_9 (= x_53 4)) (?v_10 (= x_53 3)) (?v_11 (= x_53 2))) (let ((?v_238 (ite ?v_9 x_7 (ite ?v_10 x_6 (ite ?v_11 x_5 x_4)))) (?v_155 (not x_8)) (?v_156 (not x_9)) (?v_157 (not x_10)) (?v_158 (not x_11)) (?v_228 (< x_48 1)) (?v_237 (or (or (or x_8 x_9) x_10) x_11)) (?v_71 (not (< x_26 (+ x_34 24)))) (?v_73 (not (< x_27 (+ x_34 27)))) (?v_75 (not (< x_28 (+ x_34 30)))) (?v_77 (not (< x_29 (+ x_34 33)))) (?v_23 (= x_43 0)) (?v_24 (= x_43 1)) (?v_32 (= x_43 2)) (?v_25 (= x_44 0)) (?v_26 (= x_44 1)) (?v_34 (= x_44 2)) (?v_27 (= x_45 0)) (?v_28 (= x_45 1)) (?v_36 (= x_45 2)) (?v_29 (= x_46 0)) (?v_30 (= x_46 1)) (?v_38 (= x_46 2))) (let ((?v_55 (not ?v_24)) (?v_47 (not ?v_32)) (?v_56 (not ?v_26)) (?v_49 (not ?v_34)) (?v_58 (not ?v_28)) (?v_51 (not ?v_36)) (?v_60 (not ?v_30)) (?v_53 (not ?v_38)) (?v_81 (= x_45 3))) (let ((?v_86 (not ?v_81)) (?v_93 (not (= x_25 x_28))) (?v_82 (= x_46 3))) (let ((?v_87 (not ?v_82)) (?v_95 (not (= x_25 x_29))) (?v_57 (not (<= (- x_27 x_26) x_14))) (?v_59 (not (<= (- x_28 x_26) x_14))) (?v_61 (not (<= (- x_29 x_26) x_14))) (?v_62 (not (<= (- x_28 x_27) x_14))) (?v_63 (not (<= (- x_29 x_27) x_14))) (?v_64 (not (<= (- x_29 x_28) x_14))) (?v_66 (not (< (- x_26 x_34) 12))) (?v_48 (<= (- x_26 x_25) 12)) (?v_67 (not (< (- x_27 x_34) 15))) (?v_50 (<= (- x_27 x_25) 15)) (?v_68 (not (< (- x_28 x_34) 18))) (?v_52 (<= (- x_28 x_25) 18)) (?v_69 (not (< (- x_29 x_34) 21))) (?v_54 (<= (- x_29 x_25) 21)) (?v_90 (= x_60 x_61)) (?v_92 (= x_60 x_62)) (?v_94 (= x_60 x_63)) (?v_97 (= x_61 x_60)) (?v_101 (= x_61 x_62)) (?v_104 (= x_61 x_63)) (?v_106 (= x_62 x_60)) (?v_107 (= x_62 x_61)) (?v_108 (= x_62 x_63)) (?v_109 (= x_63 x_60)) (?v_110 (= x_63 x_61)) (?v_111 (= x_63 x_62)) (?v_88 (not (< x_25 x_26))) (?v_124 (= x_60 x_65)) (?v_96 (not (< x_25 x_27))) (?v_126 (= x_61 x_65)) (?v_100 (not (< x_25 x_28))) (?v_128 (= x_62 x_65)) (?v_103 (not (< x_25 x_29))) (?v_130 (= x_63 x_65)) (?v_78 (= x_43 3))) (let ((?v_83 (not ?v_78)) (?v_89 (not (= x_25 x_26))) (?v_79 (= x_44 3))) (let ((?v_84 (not ?v_79)) (?v_91 (not (= x_25 x_27))) (?v_70 (and x_30 (< x_35 x_26))) (?v_72 (and x_31 (< x_35 x_27))) (?v_74 (and x_32 (< x_35 x_28))) (?v_76 (and x_33 (< x_35 x_29))) (?v_16 (= x_65 4)) (?v_17 (= x_65 3)) (?v_18 (= x_65 2))) (let ((?v_122 (ite ?v_16 x_29 (ite ?v_17 x_28 (ite ?v_18 x_27 x_26)))) (?v_39 (not x_30)) (?v_40 (not x_31)) (?v_41 (not x_32)) (?v_42 (not x_33)) (?v_112 (< x_60 1)) (?v_121 (or (or (or x_30 x_31) x_32) x_33)) (?v_268 (and (= x_59 x_47) (= x_35 x_13)))) (let ((?v_304 (and ?v_268 (= x_30 x_8))) (?v_269 (= x_31 x_9))) (let ((?v_320 (and ?v_304 ?v_269)) (?v_270 (= x_32 x_10))) (let ((?v_333 (and ?v_320 ?v_270)) (?v_271 (= x_33 x_11)) (?v_272 (= x_65 x_53)) (?v_273 (= x_34 x_12))) (let ((?v_266 (and (and (and ?v_333 ?v_271) ?v_272) ?v_273)) (?v_313 (= x_61 x_53)) (?v_328 (= x_62 x_53)) (?v_261 (= x_25 x_4)) (?v_262 (= x_25 x_5)) (?v_263 (= x_25 x_6)) (?v_264 (= x_25 x_7)) (?v_341 (= x_63 x_53)) (?v_283 (= x_47 1)) (?v_278 (= x_3 x_13)) (?v_300 (= x_46 x_24)) (?v_301 (= x_63 x_51)) (?v_302 (= x_29 x_7)) (?v_274 (= x_43 x_21)) (?v_267 (= x_60 x_48)) (?v_275 (= x_26 x_4)) (?v_294 (= x_44 x_22)) (?v_295 (= x_61 x_49)) (?v_296 (= x_27 x_5)) (?v_297 (= x_45 x_23)) (?v_298 (= x_62 x_50)) (?v_299 (= x_28 x_6)) (?v_285 (= x_60 x_53)) (?v_6 (and (and (and ?v_155 ?v_156) ?v_157) ?v_158)) (?v_276 (= x_30 false))) (let ((?v_280 (and (and (and (and (and (and ?v_268 ?v_276) ?v_269) ?v_270) ?v_271) ?v_272) ?v_273)) (?v_306 (= x_31 false))) (let ((?v_308 (and (and (and (and (and ?v_304 ?v_306) ?v_270) ?v_271) ?v_272) ?v_273)) (?v_322 (= x_32 false))) (let ((?v_324 (and (and (and (and ?v_320 ?v_322) ?v_271) ?v_272) ?v_273)) (?v_335 (= x_33 false))) (let ((?v_337 (and (and (and ?v_333 ?v_335) ?v_272) ?v_273)) (?v_12 (= x_48 4)) (?v_13 (= x_49 4)) (?v_14 (= x_50 4)) (?v_15 (= x_51 4)) (?v_19 (= x_60 4)) (?v_20 (= x_61 4)) (?v_21 (= x_62 4)) (?v_22 (= x_63 4)) (?v_2 (not x_0)) (?v_0 (not x_1))) (let ((?v_3 (and ?v_2 ?v_0)) (?v_1 (not x_2))) (let ((?v_345 (and ?v_3 ?v_1)) (?v_4 (and x_0 ?v_0))) (let ((?v_346 (and ?v_4 ?v_1)) (?v_347 (and (and ?v_2 x_1) ?v_1)) (?v_348 (and (and x_0 x_1) ?v_1)) (?v_349 (and ?v_3 x_2)) (?v_350 (and ?v_4 x_2)) (?v_5 (and (and (and ?v_39 ?v_40) ?v_41) ?v_42)) (?v_357 (not (= x_36 0))) (?v_358 (not (= x_36 1))) (?v_359 (not (= x_36 2))) (?v_360 (not (= x_36 3))) (?v_361 (not (= x_36 4))) (?v_362 (not (= x_36 5))) (?v_7 (or x_0 x_1))) (let ((?v_351 (or ?v_7 x_2)) (?v_8 (or ?v_2 x_1))) (let ((?v_352 (or ?v_8 x_2)) (?v_353 (or (or x_0 ?v_0) x_2)) (?v_354 (or (or ?v_2 ?v_0) x_2)) (?v_355 (or ?v_7 ?v_1)) (?v_356 (or ?v_8 ?v_1)) (?v_247 (ite ?v_12 1 (+ x_48 1))) (?v_248 (ite ?v_13 1 (+ x_49 1))) (?v_249 (ite ?v_14 1 (+ x_50 1))) (?v_250 (ite ?v_15 1 (+ x_51 1))) (?v_131 (ite ?v_19 1 (+ x_60 1))) (?v_132 (ite ?v_20 1 (+ x_61 1))) (?v_133 (ite ?v_21 1 (+ x_62 1))) (?v_134 (ite ?v_22 1 (+ x_63 1))) (?v_31 (or ?v_23 ?v_24)) (?v_33 (or ?v_25 ?v_26)) (?v_35 (or ?v_27 ?v_28)) (?v_37 (or ?v_29 ?v_30)) (?v_65 (and (and ?v_121 (= x_59 0)) (= x_66 2)))) (let ((?v_43 (or ?v_31 ?v_32)) (?v_44 (or ?v_33 ?v_34)) (?v_45 (or ?v_35 ?v_36)) (?v_46 (or ?v_37 ?v_38)) (?v_135 (or (or ?v_55 ?v_70) ?v_71)) (?v_136 (or (or ?v_56 ?v_72) ?v_73)) (?v_137 (or (or ?v_58 ?v_74) ?v_75)) (?v_138 (or (or ?v_60 ?v_76) ?v_77)) (?v_123 (or ?v_83 ?v_88)) (?v_125 (or ?v_84 ?v_96)) (?v_127 (or ?v_86 ?v_100)) (?v_129 (or ?v_87 ?v_103)) (?v_114 (and ?v_55 ?v_47)) (?v_115 (and ?v_56 ?v_49)) (?v_117 (and ?v_58 ?v_51)) (?v_118 (and ?v_60 ?v_53)) (?v_147 (or ?v_139 ?v_140)) (?v_149 (or ?v_141 ?v_142)) (?v_151 (or ?v_143 ?v_144)) (?v_153 (or ?v_145 ?v_146)) (?v_181 (and (and ?v_237 ?v_277) (= x_54 2)))) (let ((?v_159 (or ?v_147 ?v_148)) (?v_160 (or ?v_149 ?v_150)) (?v_161 (or ?v_151 ?v_152)) (?v_162 (or ?v_153 ?v_154)) (?v_251 (or (or ?v_171 ?v_186) ?v_187)) (?v_252 (or (or ?v_172 ?v_188) ?v_189)) (?v_253 (or (or ?v_174 ?v_190) ?v_191)) (?v_254 (or (or ?v_176 ?v_192) ?v_193)) (?v_239 (or ?v_199 ?v_204)) (?v_241 (or ?v_200 ?v_212)) (?v_243 (or ?v_202 ?v_216)) (?v_245 (or ?v_203 ?v_219)) (?v_230 (and ?v_171 ?v_163)) (?v_231 (and ?v_172 ?v_165)) (?v_233 (and ?v_174 ?v_167)) (?v_234 (and ?v_176 ?v_169)) (?v_259 (and (and (and ?v_255 ?v_256) ?v_257) ?v_258)) (?v_260 (and (and (and (<= x_25 x_4) (<= x_25 x_5)) (<= x_25 x_6)) (<= x_25 x_7))) (?v_279 (+ x_3 12))) (let ((?v_281 (= x_26 ?v_279)) (?v_288 (= x_35 (ite ?v_6 (+ x_3 x_14) x_13)))) (let ((?v_305 (and (= x_59 (ite ?v_6 0 x_47)) ?v_288)) (?v_289 (= x_31 (or ?v_6 x_9))) (?v_290 (= x_32 (or ?v_6 x_10))) (?v_291 (= x_33 (or ?v_6 x_11))) (?v_292 (= x_65 (ite ?v_6 1 x_53))) (?v_293 (= x_34 (ite ?v_6 x_3 x_12)))) (let ((?v_282 (and (and (and (and (and (and ?v_305 ?v_276) ?v_289) ?v_290) ?v_291) ?v_292) ?v_293)) (?v_201 (+ x_3 3))) (let ((?v_309 (- ?v_201 x_14))) (let ((?v_284 (= x_26 ?v_309)) (?v_286 (and ?v_194 ?v_265)) (?v_287 (= x_26 ?v_201)) (?v_316 (= x_59 (ite ?v_6 1 x_47))) (?v_307 (+ x_3 15))) (let ((?v_310 (= x_27 ?v_307)) (?v_317 (= x_30 (or ?v_6 x_8)))) (let ((?v_321 (and ?v_305 ?v_317)) (?v_318 (= x_65 (ite ?v_6 2 x_53)))) (let ((?v_311 (and (and (and (and (and ?v_321 ?v_306) ?v_290) ?v_291) ?v_318) ?v_293)) (?v_312 (= x_27 ?v_309)) (?v_314 (and ?v_195 ?v_303)) (?v_315 (= x_27 ?v_201)) (?v_323 (+ x_3 18))) (let ((?v_325 (= x_28 ?v_323)) (?v_334 (and ?v_321 ?v_289)) (?v_331 (= x_65 (ite ?v_6 3 x_53)))) (let ((?v_326 (and (and (and (and ?v_334 ?v_322) ?v_291) ?v_331) ?v_293)) (?v_327 (= x_28 ?v_309)) (?v_329 (and ?v_197 ?v_319)) (?v_330 (= x_28 ?v_201)) (?v_336 (+ x_3 21))) (let ((?v_338 (= x_29 ?v_336)) (?v_344 (= x_65 (ite ?v_6 4 x_53)))) (let ((?v_339 (and (and (and (and ?v_334 ?v_290) ?v_335) ?v_344) ?v_293)) (?v_340 (= x_29 ?v_309)) (?v_342 (and ?v_198 ?v_332)) (?v_343 (= x_29 ?v_201)) (?v_80 (+ x_34 3)) (?v_85 (+ x_25 3)) (?v_98 (+ x_26 3)) (?v_99 (+ x_27 3)) (?v_102 (+ x_28 3)) (?v_105 (+ x_29 3)) (?v_113 (+ (+ x_26 (* (ite ?v_112 (- (- 1 x_60) 1) (- (+ (- 4 x_60) 1) 1)) 3)) x_14)) (?v_116 (+ (+ x_27 (* (ite (< x_61 2) (- (- 2 x_61) 1) (- (+ (- 4 x_61) 2) 1)) 3)) x_14)) (?v_119 (+ (+ x_28 (* (ite (< x_62 3) (- (- 3 x_62) 1) (- (+ (- 4 x_62) 3) 1)) 3)) x_14)) (?v_120 (+ (+ x_29 (* (ite (< x_63 4) (- x_64 1) (- (+ x_64 4) 1)) 3)) x_14)) (?v_196 (+ x_12 3)) (?v_214 (+ x_4 3)) (?v_215 (+ x_5 3)) (?v_218 (+ x_6 3)) (?v_221 (+ x_7 3)) (?v_229 (+ (+ x_4 (* (ite ?v_228 (- (- 1 x_48) 1) (- (+ (- 4 x_48) 1) 1)) 3)) x_14)) (?v_232 (+ (+ x_5 (* (ite (< x_49 2) (- (- 2 x_49) 1) (- (+ (- 4 x_49) 2) 1)) 3)) x_14)) (?v_235 (+ (+ x_6 (* (ite (< x_50 3) (- (- 3 x_50) 1) (- (+ (- 4 x_50) 3) 1)) 3)) x_14)) (?v_236 (+ (+ x_7 (* (ite (< x_51 4) (- x_52 1) (- (+ x_52 4) 1)) 3)) x_14))) (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (<= x_71 6) (>= x_71 0)) (<= x_66 3)) (>= x_66 0)) (<= x_59 1)) (>= x_59 0)) (<= x_54 3)) (>= x_54 0)) (<= x_47 1)) (>= x_47 0)) (<= x_46 3)) (>= x_46 0)) (<= x_45 3)) (>= x_45 0)) (<= x_44 3)) (>= x_44 0)) (<= x_43 3)) (>= x_43 0)) (<= x_36 6)) (>= x_36 0)) (<= x_24 3)) (>= x_24 0)) (<= x_23 3)) (>= x_23 0)) (<= x_22 3)) (>= x_22 0)) (<= x_21 3)) (>= x_21 0)) (> x_14 0)) (< x_14 (/ 3 2))) (or (or (or (= x_48 1) (= x_48 2)) (= x_48 3)) ?v_12)) (not ?v_228)) (<= x_48 4)) (or (or (or (= x_49 1) (= x_49 2)) (= x_49 3)) ?v_13)) (not (< x_49 1))) (<= x_49 4)) (or (or (or (= x_50 1) (= x_50 2)) (= x_50 3)) ?v_14)) (not (< x_50 1))) (<= x_50 4)) (or (or (or (= x_51 1) (= x_51 2)) (= x_51 3)) ?v_15)) (not (< x_51 1))) (<= x_51 4)) (or (or (or (= x_53 1) ?v_11) ?v_10) ?v_9)) (not (< x_53 1))) (<= x_53 4)) (or (or (or (= x_60 1) (= x_60 2)) (= x_60 3)) ?v_19)) (not ?v_112)) (<= x_60 4)) (or (or (or (= x_61 1) (= x_61 2)) (= x_61 3)) ?v_20)) (not (< x_61 1))) (<= x_61 4)) (or (or (or (= x_62 1) (= x_62 2)) (= x_62 3)) ?v_21)) (not (< x_62 1))) (<= x_62 4)) (or (or (or (= x_63 1) (= x_63 2)) (= x_63 3)) ?v_22)) (not (< x_63 1))) (<= x_63 4)) (or (or (or (= x_65 1) ?v_18) ?v_17) ?v_16)) (not (< x_65 1))) (<= x_65 4)) (or (or (or (or (or ?v_345 ?v_346) ?v_347) ?v_348) ?v_349) ?v_350)) (<= x_25 x_26)) (<= x_25 x_27)) (<= x_25 x_28)) (<= x_25 x_29)) (or ?v_5 (and (<= x_34 x_25) (<= x_25 x_35)))) (or (= x_35 (+ x_34 x_14)) ?v_5)) (or ?v_357 x_37)) (or ?v_358 x_38)) (or ?v_359 x_39)) (or ?v_360 x_40)) (or ?v_361 x_41)) (or ?v_362 x_42)) (or (or (or ?v_78 ?v_23) ?v_24) ?v_32)) (or (or (or ?v_79 ?v_25) ?v_26) ?v_34)) (or (or (or ?v_81 ?v_27) ?v_28) ?v_36)) (or (or (or ?v_82 ?v_29) ?v_30) ?v_38)) (<= x_3 x_4)) (<= x_3 x_5)) (<= x_3 x_6)) (<= x_3 x_7)) (or ?v_6 (and (<= x_12 x_3) (<= x_3 x_13)))) (or (= x_13 (+ x_12 x_14)) ?v_6)) (or ?v_351 x_15)) (or ?v_352 x_16)) (or ?v_353 x_17)) (or ?v_354 x_18)) (or ?v_355 x_19)) (or ?v_356 x_20)) (or (or (or ?v_194 ?v_139) ?v_140) ?v_148)) (or (or (or ?v_195 ?v_141) ?v_142) ?v_150)) (or (or (or ?v_197 ?v_143) ?v_144) ?v_152)) (or (or (or ?v_198 ?v_145) ?v_146) ?v_154)) (= x_52 (- 4 x_51))) (= x_54 (ite ?v_9 x_24 (ite ?v_10 x_23 (ite ?v_11 x_22 x_21))))) (= x_55 ?v_247)) (= x_56 ?v_248)) (= x_57 ?v_249)) (= x_58 ?v_250)) (= x_64 (- 4 x_63))) (= x_66 (ite ?v_16 x_46 (ite ?v_17 x_45 (ite ?v_18 x_44 x_43))))) (= x_67 ?v_131)) (= x_68 ?v_132)) (= x_69 ?v_133)) (= x_70 ?v_134)) (= x_37 (and (and (and (and ?v_5 ?v_31) ?v_33) ?v_35) ?v_37))) (= x_38 (and (and (and (and (and (and (and (and (and (and (and (and ?v_65 ?v_43) ?v_44) ?v_45) ?v_46) (or ?v_47 (and (and ?v_39 ?v_66) ?v_48))) (or ?v_49 (and (and ?v_40 ?v_67) ?v_50))) (or ?v_51 (and (and ?v_41 ?v_68) ?v_52))) (or ?v_53 (and (and ?v_42 ?v_69) ?v_54))) (or (or ?v_55 x_30) ?v_71)) (or (or ?v_56 x_31) ?v_73)) (or (or ?v_58 x_32) ?v_75)) (or (or ?v_60 x_33) ?v_77)))) (= x_39 (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and ?v_5 (or (or (or ?v_32 ?v_34) ?v_36) ?v_38)) ?v_43) ?v_44) ?v_45) ?v_46) (or ?v_47 ?v_48)) (or ?v_49 ?v_50)) (or ?v_51 ?v_52)) (or ?v_53 ?v_54)) (or ?v_47 (and (and (or ?v_49 ?v_57) (or ?v_51 ?v_59)) (or ?v_53 ?v_61)))) (or ?v_49 (and (or ?v_51 ?v_62) (or ?v_53 ?v_63)))) (or (or ?v_51 ?v_53) ?v_64)) (or ?v_47 (and (and (and (or ?v_55 (not (<= (- x_26 x_26) x_14))) (or ?v_56 ?v_57)) (or ?v_58 ?v_59)) (or ?v_60 ?v_61)))) (or ?v_49 (and (and (and (or ?v_55 (not (<= (- x_26 x_27) x_14))) (or ?v_56 (not (<= (- x_27 x_27) x_14)))) (or ?v_58 ?v_62)) (or ?v_60 ?v_63)))) (or ?v_51 (and (and (and (or ?v_55 (not (<= (- x_26 x_28) x_14))) (or ?v_56 (not (<= (- x_27 x_28) x_14)))) (or ?v_58 (not (<= (- x_28 x_28) x_14)))) (or ?v_60 ?v_64)))) (or ?v_53 (and (and (and (or ?v_55 (not (<= (- x_26 x_29) x_14))) (or ?v_56 (not (<= (- x_27 x_29) x_14)))) (or ?v_58 (not (<= (- x_28 x_29) x_14)))) (or ?v_60 (not (<= (- x_29 x_29) x_14)))))))) (= x_40 (and (and (and (and (and (and (and (and (and (and (and (and (and (and ?v_65 (= ?v_122 (+ (+ x_34 (* (- x_65 1) 3)) 12))) (not (ite ?v_16 x_33 (ite ?v_17 x_32 (ite ?v_18 x_31 x_30))))) (or (or (or ?v_47 (= 1 x_65)) ?v_70) (and ?v_66 ?v_48))) (or (or (or ?v_49 (= 2 x_65)) ?v_72) (and ?v_67 ?v_50))) (or (or (or ?v_51 (= 3 x_65)) ?v_74) (and ?v_68 ?v_52))) (or (or (or ?v_53 (= 4 x_65)) ?v_76) (and ?v_69 ?v_54))) ?v_135) ?v_136) ?v_137) ?v_138) (or ?v_83 (and ?v_124 (= x_26 ?v_80)))) (or ?v_84 (and ?v_126 (= x_27 ?v_80)))) (or ?v_86 (and ?v_128 (= x_28 ?v_80)))) (or ?v_87 (and ?v_130 (= x_29 ?v_80)))))) (= x_41 (and (and (and (and (and (and (and (and (and (and (and (and (and ?v_5 (or (or (or ?v_78 ?v_79) ?v_81) ?v_82)) (or ?v_83 (<= x_26 ?v_85))) (or ?v_84 (<= x_27 ?v_85))) (or ?v_86 (<= x_28 ?v_85))) (or ?v_87 (<= x_29 ?v_85))) (or ?v_83 (and (and (and (or (or ?v_123 ?v_89) (and (= x_60 x_67) (= x_26 ?v_98))) (or ?v_84 (and (and (or (or ?v_88 ?v_96) (and ?v_90 (= x_26 x_27))) (or (or ?v_89 ?v_91) ?v_90)) (or (or ?v_88 ?v_91) (and (= x_60 x_68) (= x_26 ?v_99)))))) (or ?v_86 (and (and (or (or ?v_88 ?v_100) (and ?v_92 (= x_26 x_28))) (or (or ?v_89 ?v_93) ?v_92)) (or (or ?v_88 ?v_93) (and (= x_60 x_69) (= x_26 ?v_102)))))) (or ?v_87 (and (and (or (or ?v_88 ?v_103) (and ?v_94 (= x_26 x_29))) (or (or ?v_89 ?v_95) ?v_94)) (or (or ?v_88 ?v_95) (and (= x_60 x_70) (= x_26 ?v_105)))))))) (or ?v_84 (and (and (and (or ?v_83 (and (and (or (or ?v_96 ?v_88) (and ?v_97 (= x_27 x_26))) (or (or ?v_91 ?v_89) ?v_97)) (or (or ?v_96 ?v_89) (and (= x_61 x_67) (= x_27 ?v_98))))) (or (or ?v_125 ?v_91) (and (= x_61 x_68) (= x_27 ?v_99)))) (or ?v_86 (and (and (or (or ?v_96 ?v_100) (and ?v_101 (= x_27 x_28))) (or (or ?v_91 ?v_93) ?v_101)) (or (or ?v_96 ?v_93) (and (= x_61 x_69) (= x_27 ?v_102)))))) (or ?v_87 (and (and (or (or ?v_96 ?v_103) (and ?v_104 (= x_27 x_29))) (or (or ?v_91 ?v_95) ?v_104)) (or (or ?v_96 ?v_95) (and (= x_61 x_70) (= x_27 ?v_105)))))))) (or ?v_86 (and (and (and (or ?v_83 (and (and (or (or ?v_100 ?v_88) (and ?v_106 (= x_28 x_26))) (or (or ?v_93 ?v_89) ?v_106)) (or (or ?v_100 ?v_89) (and (= x_62 x_67) (= x_28 ?v_98))))) (or ?v_84 (and (and (or (or ?v_100 ?v_96) (and ?v_107 (= x_28 x_27))) (or (or ?v_93 ?v_91) ?v_107)) (or (or ?v_100 ?v_91) (and (= x_62 x_68) (= x_28 ?v_99)))))) (or (or ?v_127 ?v_93) (and (= x_62 x_69) (= x_28 ?v_102)))) (or ?v_87 (and (and (or (or ?v_100 ?v_103) (and ?v_108 (= x_28 x_29))) (or (or ?v_93 ?v_95) ?v_108)) (or (or ?v_100 ?v_95) (and (= x_62 x_70) (= x_28 ?v_105)))))))) (or ?v_87 (and (and (and (or ?v_83 (and (and (or (or ?v_103 ?v_88) (and ?v_109 (= x_29 x_26))) (or (or ?v_95 ?v_89) ?v_109)) (or (or ?v_103 ?v_89) (and (= x_63 x_67) (= x_29 ?v_98))))) (or ?v_84 (and (and (or (or ?v_103 ?v_96) (and ?v_110 (= x_29 x_27))) (or (or ?v_95 ?v_91) ?v_110)) (or (or ?v_103 ?v_91) (and (= x_63 x_68) (= x_29 ?v_99)))))) (or ?v_86 (and (and (or (or ?v_103 ?v_100) (and ?v_111 (= x_29 x_28))) (or (or ?v_95 ?v_93) ?v_111)) (or (or ?v_103 ?v_93) (and (= x_63 x_69) (= x_29 ?v_102)))))) (or (or ?v_129 ?v_95) (and (= x_63 x_70) (= x_29 ?v_105)))))) (or ?v_83 (and (and (and (or ?v_114 (not (<= x_26 ?v_113))) (or ?v_115 (not (<= x_27 ?v_113)))) (or ?v_117 (not (<= x_28 ?v_113)))) (or ?v_118 (not (<= x_29 ?v_113)))))) (or ?v_84 (and (and (and (or ?v_114 (not (<= x_26 ?v_116))) (or ?v_115 (not (<= x_27 ?v_116)))) (or ?v_117 (not (<= x_28 ?v_116)))) (or ?v_118 (not (<= x_29 ?v_116)))))) (or ?v_86 (and (and (and (or ?v_114 (not (<= x_26 ?v_119))) (or ?v_115 (not (<= x_27 ?v_119)))) (or ?v_117 (not (<= x_28 ?v_119)))) (or ?v_118 (not (<= x_29 ?v_119)))))) (or ?v_87 (and (and (and (or ?v_114 (not (<= x_26 ?v_120))) (or ?v_115 (not (<= x_27 ?v_120)))) (or ?v_117 (not (<= x_28 ?v_120)))) (or ?v_118 (not (<= x_29 ?v_120)))))))) (= x_42 (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and ?v_121 (= x_59 1)) (= x_66 3)) (= (ite ?v_16 x_63 (ite ?v_17 x_62 (ite ?v_18 x_61 x_60))) x_65)) (= ?v_122 ?v_80)) (or ?v_123 (and ?v_124 (= x_26 ?v_122)))) (or ?v_125 (and ?v_126 (= x_27 ?v_122)))) (or ?v_127 (and ?v_128 (= x_28 ?v_122)))) (or ?v_129 (and ?v_130 (= x_29 ?v_122)))) (or (or ?v_83 ?v_89) (and (= ?v_131 x_65) (= ?v_122 ?v_98)))) (or (or ?v_84 ?v_91) (and (= ?v_132 x_65) (= ?v_122 ?v_99)))) (or (or ?v_86 ?v_93) (and (= ?v_133 x_65) (= ?v_122 ?v_102)))) (or (or ?v_87 ?v_95) (and (= ?v_134 x_65) (= ?v_122 ?v_105)))) ?v_135) ?v_136) ?v_137) ?v_138) (or ?v_47 ?v_70)) (or ?v_49 ?v_72)) (or ?v_51 ?v_74)) (or ?v_53 ?v_76)))) (= x_15 (and (and (and (and ?v_6 ?v_147) ?v_149) ?v_151) ?v_153))) (= x_16 (and (and (and (and (and (and (and (and (and (and (and (and ?v_181 ?v_159) ?v_160) ?v_161) ?v_162) (or ?v_163 (and (and ?v_155 ?v_182) ?v_164))) (or ?v_165 (and (and ?v_156 ?v_183) ?v_166))) (or ?v_167 (and (and ?v_157 ?v_184) ?v_168))) (or ?v_169 (and (and ?v_158 ?v_185) ?v_170))) (or (or ?v_171 x_8) ?v_187)) (or (or ?v_172 x_9) ?v_189)) (or (or ?v_174 x_10) ?v_191)) (or (or ?v_176 x_11) ?v_193)))) (= x_17 (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and ?v_6 (or (or (or ?v_148 ?v_150) ?v_152) ?v_154)) ?v_159) ?v_160) ?v_161) ?v_162) (or ?v_163 ?v_164)) (or ?v_165 ?v_166)) (or ?v_167 ?v_168)) (or ?v_169 ?v_170)) (or ?v_163 (and (and (or ?v_165 ?v_173) (or ?v_167 ?v_175)) (or ?v_169 ?v_177)))) (or ?v_165 (and (or ?v_167 ?v_178) (or ?v_169 ?v_179)))) (or (or ?v_167 ?v_169) ?v_180)) (or ?v_163 (and (and (and (or ?v_171 (not (<= (- x_4 x_4) x_14))) (or ?v_172 ?v_173)) (or ?v_174 ?v_175)) (or ?v_176 ?v_177)))) (or ?v_165 (and (and (and (or ?v_171 (not (<= (- x_4 x_5) x_14))) (or ?v_172 (not (<= (- x_5 x_5) x_14)))) (or ?v_174 ?v_178)) (or ?v_176 ?v_179)))) (or ?v_167 (and (and (and (or ?v_171 (not (<= (- x_4 x_6) x_14))) (or ?v_172 (not (<= (- x_5 x_6) x_14)))) (or ?v_174 (not (<= (- x_6 x_6) x_14)))) (or ?v_176 ?v_180)))) (or ?v_169 (and (and (and (or ?v_171 (not (<= (- x_4 x_7) x_14))) (or ?v_172 (not (<= (- x_5 x_7) x_14)))) (or ?v_174 (not (<= (- x_6 x_7) x_14)))) (or ?v_176 (not (<= (- x_7 x_7) x_14)))))))) (= x_18 (and (and (and (and (and (and (and (and (and (and (and (and (and (and ?v_181 (= ?v_238 (+ (+ x_12 (* (- x_53 1) 3)) 12))) (not (ite ?v_9 x_11 (ite ?v_10 x_10 (ite ?v_11 x_9 x_8))))) (or (or (or ?v_163 (= 1 x_53)) ?v_186) (and ?v_182 ?v_164))) (or (or (or ?v_165 (= 2 x_53)) ?v_188) (and ?v_183 ?v_166))) (or (or (or ?v_167 (= 3 x_53)) ?v_190) (and ?v_184 ?v_168))) (or (or (or ?v_169 (= 4 x_53)) ?v_192) (and ?v_185 ?v_170))) ?v_251) ?v_252) ?v_253) ?v_254) (or ?v_199 (and ?v_240 (= x_4 ?v_196)))) (or ?v_200 (and ?v_242 (= x_5 ?v_196)))) (or ?v_202 (and ?v_244 (= x_6 ?v_196)))) (or ?v_203 (and ?v_246 (= x_7 ?v_196)))))) (= x_19 (and (and (and (and (and (and (and (and (and (and (and (and (and ?v_6 (or (or (or ?v_194 ?v_195) ?v_197) ?v_198)) (or ?v_199 (<= x_4 ?v_201))) (or ?v_200 (<= x_5 ?v_201))) (or ?v_202 (<= x_6 ?v_201))) (or ?v_203 (<= x_7 ?v_201))) (or ?v_199 (and (and (and (or (or ?v_239 ?v_205) (and (= x_48 x_55) (= x_4 ?v_214))) (or ?v_200 (and (and (or (or ?v_204 ?v_212) (and ?v_206 (= x_4 x_5))) (or (or ?v_205 ?v_207) ?v_206)) (or (or ?v_204 ?v_207) (and (= x_48 x_56) (= x_4 ?v_215)))))) (or ?v_202 (and (and (or (or ?v_204 ?v_216) (and ?v_208 (= x_4 x_6))) (or (or ?v_205 ?v_209) ?v_208)) (or (or ?v_204 ?v_209) (and (= x_48 x_57) (= x_4 ?v_218)))))) (or ?v_203 (and (and (or (or ?v_204 ?v_219) (and ?v_210 (= x_4 x_7))) (or (or ?v_205 ?v_211) ?v_210)) (or (or ?v_204 ?v_211) (and (= x_48 x_58) (= x_4 ?v_221)))))))) (or ?v_200 (and (and (and (or ?v_199 (and (and (or (or ?v_212 ?v_204) (and ?v_213 (= x_5 x_4))) (or (or ?v_207 ?v_205) ?v_213)) (or (or ?v_212 ?v_205) (and (= x_49 x_55) (= x_5 ?v_214))))) (or (or ?v_241 ?v_207) (and (= x_49 x_56) (= x_5 ?v_215)))) (or ?v_202 (and (and (or (or ?v_212 ?v_216) (and ?v_217 (= x_5 x_6))) (or (or ?v_207 ?v_209) ?v_217)) (or (or ?v_212 ?v_209) (and (= x_49 x_57) (= x_5 ?v_218)))))) (or ?v_203 (and (and (or (or ?v_212 ?v_219) (and ?v_220 (= x_5 x_7))) (or (or ?v_207 ?v_211) ?v_220)) (or (or ?v_212 ?v_211) (and (= x_49 x_58) (= x_5 ?v_221)))))))) (or ?v_202 (and (and (and (or ?v_199 (and (and (or (or ?v_216 ?v_204) (and ?v_222 (= x_6 x_4))) (or (or ?v_209 ?v_205) ?v_222)) (or (or ?v_216 ?v_205) (and (= x_50 x_55) (= x_6 ?v_214))))) (or ?v_200 (and (and (or (or ?v_216 ?v_212) (and ?v_223 (= x_6 x_5))) (or (or ?v_209 ?v_207) ?v_223)) (or (or ?v_216 ?v_207) (and (= x_50 x_56) (= x_6 ?v_215)))))) (or (or ?v_243 ?v_209) (and (= x_50 x_57) (= x_6 ?v_218)))) (or ?v_203 (and (and (or (or ?v_216 ?v_219) (and ?v_224 (= x_6 x_7))) (or (or ?v_209 ?v_211) ?v_224)) (or (or ?v_216 ?v_211) (and (= x_50 x_58) (= x_6 ?v_221)))))))) (or ?v_203 (and (and (and (or ?v_199 (and (and (or (or ?v_219 ?v_204) (and ?v_225 (= x_7 x_4))) (or (or ?v_211 ?v_205) ?v_225)) (or (or ?v_219 ?v_205) (and (= x_51 x_55) (= x_7 ?v_214))))) (or ?v_200 (and (and (or (or ?v_219 ?v_212) (and ?v_226 (= x_7 x_5))) (or (or ?v_211 ?v_207) ?v_226)) (or (or ?v_219 ?v_207) (and (= x_51 x_56) (= x_7 ?v_215)))))) (or ?v_202 (and (and (or (or ?v_219 ?v_216) (and ?v_227 (= x_7 x_6))) (or (or ?v_211 ?v_209) ?v_227)) (or (or ?v_219 ?v_209) (and (= x_51 x_57) (= x_7 ?v_218)))))) (or (or ?v_245 ?v_211) (and (= x_51 x_58) (= x_7 ?v_221)))))) (or ?v_199 (and (and (and (or ?v_230 (not (<= x_4 ?v_229))) (or ?v_231 (not (<= x_5 ?v_229)))) (or ?v_233 (not (<= x_6 ?v_229)))) (or ?v_234 (not (<= x_7 ?v_229)))))) (or ?v_200 (and (and (and (or ?v_230 (not (<= x_4 ?v_232))) (or ?v_231 (not (<= x_5 ?v_232)))) (or ?v_233 (not (<= x_6 ?v_232)))) (or ?v_234 (not (<= x_7 ?v_232)))))) (or ?v_202 (and (and (and (or ?v_230 (not (<= x_4 ?v_235))) (or ?v_231 (not (<= x_5 ?v_235)))) (or ?v_233 (not (<= x_6 ?v_235)))) (or ?v_234 (not (<= x_7 ?v_235)))))) (or ?v_203 (and (and (and (or ?v_230 (not (<= x_4 ?v_236))) (or ?v_231 (not (<= x_5 ?v_236)))) (or ?v_233 (not (<= x_6 ?v_236)))) (or ?v_234 (not (<= x_7 ?v_236)))))))) (= x_20 (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and ?v_237 ?v_283) (= x_54 3)) (= (ite ?v_9 x_51 (ite ?v_10 x_50 (ite ?v_11 x_49 x_48))) x_53)) (= ?v_238 ?v_196)) (or ?v_239 (and ?v_240 (= x_4 ?v_238)))) (or ?v_241 (and ?v_242 (= x_5 ?v_238)))) (or ?v_243 (and ?v_244 (= x_6 ?v_238)))) (or ?v_245 (and ?v_246 (= x_7 ?v_238)))) (or (or ?v_199 ?v_205) (and (= ?v_247 x_53) (= ?v_238 ?v_214)))) (or (or ?v_200 ?v_207) (and (= ?v_248 x_53) (= ?v_238 ?v_215)))) (or (or ?v_202 ?v_209) (and (= ?v_249 x_53) (= ?v_238 ?v_218)))) (or (or ?v_203 ?v_211) (and (= ?v_250 x_53) (= ?v_238 ?v_221)))) ?v_251) ?v_252) ?v_253) ?v_254) (or ?v_163 ?v_186)) (or ?v_165 ?v_188)) (or ?v_167 ?v_190)) (or ?v_169 ?v_192)))) (= x_71 (ite x_41 4 6))) (= x_72 ?v_247)) (= x_73 ?v_248)) (= x_74 ?v_249)) (= x_75 ?v_250)) (or (and (and (and (and (and (and (and (and (and (and (and (and (and (and (ite ?v_6 ?v_259 (and ?v_259 (< x_3 x_13))) (ite ?v_6 (and ?v_260 (or (or (or ?v_261 ?v_262) ?v_263) ?v_264)) (and (and ?v_260 (<= x_25 x_13)) (or (or (or (or (= x_25 x_13) ?v_261) ?v_262) ?v_263) ?v_264)))) ?v_274) ?v_294) ?v_297) ?v_300) ?v_267) ?v_295) ?v_298) ?v_301) ?v_275) ?v_296) ?v_299) ?v_302) ?v_266) (and (or (or (or (and (and (and (and (and (and (and (and (and (or (or (or (or (or (or (or (or (or (and (and (and (and (and ?v_139 ?v_265) ?v_24) (= x_26 (+ x_3 24))) ?v_266) ?v_267) (and (and (and (and (and (and ?v_139 x_8) ?v_278) ?v_280) ?v_274) ?v_267) ?v_275)) (and (and (and (and (and ?v_140 ?v_265) ?v_32) ?v_281) ?v_282) ?v_267)) (and (and (and (and (and (and (and ?v_140 x_8) ?v_277) ?v_278) ?v_32) (= x_26 (- ?v_279 x_14))) ?v_280) ?v_267)) (and (and (and (and (and (and (and ?v_148 x_8) ?v_277) ?v_278) ?v_78) ?v_284) ?v_285) ?v_280)) (and (and (and (and (and ?v_148 ?v_265) ?v_281) ?v_282) ?v_274) ?v_267)) (and (and (and (and (and (and (and (or ?v_140 ?v_148) x_8) ?v_283) ?v_278) ?v_78) ?v_284) ?v_285) ?v_280)) (and (and (and (and (and ?v_286 (not (= ?v_247 1))) ?v_287) (= x_60 x_72)) ?v_266) ?v_274)) (and (and (and (and (and (and (and (and (and (and (and (and ?v_286 (= x_72 1)) ?v_287) (= x_60 ?v_247)) ?v_316) ?v_288) ?v_39) ?v_289) ?v_290) ?v_291) ?v_292) ?v_293) ?v_274)) (and (and (and (and (and (and (and ?v_194 x_8) ?v_283) ?v_278) ?v_280) ?v_274) ?v_267) ?v_275)) ?v_294) ?v_295) ?v_296) ?v_297) ?v_298) ?v_299) ?v_300) ?v_301) ?v_302) (and (and (and (and (and (and (and (and (and (or (or (or (or (or (or (or (or (or (and (and (and (and (and ?v_141 ?v_303) ?v_26) (= x_27 (+ x_3 27))) ?v_266) ?v_295) (and (and (and (and (and (and ?v_141 x_9) ?v_278) ?v_308) ?v_294) ?v_295) ?v_296)) (and (and (and (and (and ?v_142 ?v_303) ?v_34) ?v_310) ?v_311) ?v_295)) (and (and (and (and (and (and (and ?v_142 x_9) ?v_277) ?v_278) ?v_34) (= x_27 (- ?v_307 x_14))) ?v_308) ?v_295)) (and (and (and (and (and (and (and ?v_150 x_9) ?v_277) ?v_278) ?v_79) ?v_312) ?v_313) ?v_308)) (and (and (and (and (and ?v_150 ?v_303) ?v_310) ?v_311) ?v_294) ?v_295)) (and (and (and (and (and (and (and (or ?v_142 ?v_150) x_9) ?v_283) ?v_278) ?v_79) ?v_312) ?v_313) ?v_308)) (and (and (and (and (and ?v_314 (not (= ?v_248 2))) ?v_315) (= x_61 x_73)) ?v_266) ?v_294)) (and (and (and (and (and (and (and (and (and (and (and (and ?v_314 (= x_73 2)) ?v_315) (= x_61 ?v_248)) ?v_316) ?v_288) ?v_317) ?v_40) ?v_290) ?v_291) ?v_318) ?v_293) ?v_294)) (and (and (and (and (and (and (and ?v_195 x_9) ?v_283) ?v_278) ?v_308) ?v_294) ?v_295) ?v_296)) ?v_274) ?v_267) ?v_275) ?v_297) ?v_298) ?v_299) ?v_300) ?v_301) ?v_302)) (and (and (and (and (and (and (and (and (and (or (or (or (or (or (or (or (or (or (and (and (and (and (and ?v_143 ?v_319) ?v_28) (= x_28 (+ x_3 30))) ?v_266) ?v_298) (and (and (and (and (and (and ?v_143 x_10) ?v_278) ?v_324) ?v_297) ?v_298) ?v_299)) (and (and (and (and (and ?v_144 ?v_319) ?v_36) ?v_325) ?v_326) ?v_298)) (and (and (and (and (and (and (and ?v_144 x_10) ?v_277) ?v_278) ?v_36) (= x_28 (- ?v_323 x_14))) ?v_324) ?v_298)) (and (and (and (and (and (and (and ?v_152 x_10) ?v_277) ?v_278) ?v_81) ?v_327) ?v_328) ?v_324)) (and (and (and (and (and ?v_152 ?v_319) ?v_325) ?v_326) ?v_297) ?v_298)) (and (and (and (and (and (and (and (or ?v_144 ?v_152) x_10) ?v_283) ?v_278) ?v_81) ?v_327) ?v_328) ?v_324)) (and (and (and (and (and ?v_329 (not (= ?v_249 3))) ?v_330) (= x_62 x_74)) ?v_266) ?v_297)) (and (and (and (and (and (and (and (and (and (and (and (and ?v_329 (= x_74 3)) ?v_330) (= x_62 ?v_249)) ?v_316) ?v_288) ?v_317) ?v_289) ?v_41) ?v_291) ?v_331) ?v_293) ?v_297)) (and (and (and (and (and (and (and ?v_197 x_10) ?v_283) ?v_278) ?v_324) ?v_297) ?v_298) ?v_299)) ?v_274) ?v_267) ?v_275) ?v_294) ?v_295) ?v_296) ?v_300) ?v_301) ?v_302)) (and (and (and (and (and (and (and (and (and (or (or (or (or (or (or (or (or (or (and (and (and (and (and ?v_145 ?v_332) ?v_30) (= x_29 (+ x_3 33))) ?v_266) ?v_301) (and (and (and (and (and (and ?v_145 x_11) ?v_278) ?v_337) ?v_300) ?v_301) ?v_302)) (and (and (and (and (and ?v_146 ?v_332) ?v_38) ?v_338) ?v_339) ?v_301)) (and (and (and (and (and (and (and ?v_146 x_11) ?v_277) ?v_278) ?v_38) (= x_29 (- ?v_336 x_14))) ?v_337) ?v_301)) (and (and (and (and (and (and (and ?v_154 x_11) ?v_277) ?v_278) ?v_82) ?v_340) ?v_341) ?v_337)) (and (and (and (and (and ?v_154 ?v_332) ?v_338) ?v_339) ?v_300) ?v_301)) (and (and (and (and (and (and (and (or ?v_146 ?v_154) x_11) ?v_283) ?v_278) ?v_82) ?v_340) ?v_341) ?v_337)) (and (and (and (and (and ?v_342 (not (= ?v_250 4))) ?v_343) (= x_63 x_75)) ?v_266) ?v_300)) (and (and (and (and (and (and (and (and (and (and (and (and ?v_342 (= x_75 4)) ?v_343) (= x_63 ?v_250)) ?v_316) ?v_288) ?v_317) ?v_289) ?v_290) ?v_42) ?v_344) ?v_293) ?v_300)) (and (and (and (and (and (and (and ?v_198 x_11) ?v_283) ?v_278) ?v_337) ?v_300) ?v_301) ?v_302)) ?v_274) ?v_267) ?v_275) ?v_294) ?v_295) ?v_296) ?v_297) ?v_298) ?v_299)) (= x_25 x_3)))) (or (or (or (or (or (or (and ?v_345 (= x_36 (ite x_37 0 (ite x_38 1 6)))) (and ?v_346 (= x_36 (ite x_38 1 (ite x_39 2 6))))) (and ?v_347 (= x_36 (ite x_39 2 (ite x_40 3 6))))) (and ?v_348 (= x_36 (ite x_40 3 (ite x_39 2 x_71))))) (and ?v_349 (= x_36 (ite x_41 4 (ite x_42 5 6))))) (and ?v_350 (= x_36 (ite x_42 5 x_71)))) (and (and (and (and (and (and ?v_351 ?v_352) ?v_353) ?v_354) ?v_355) ?v_356) (= x_36 6)))) ?v_357) ?v_358) ?v_359) ?v_360) ?v_361) ?v_362) (or (or ?v_1 ?v_0) ?v_2)))))))))))))))))))))))))))))))))))))))))))))))))))
(check-sat)
(exit)
