(set-info :source |fuzzsmt|)
(set-info :smt-lib-version 2.0)
(set-info :category "random")
(set-info :status unknown)
(set-logic QF_LIA)
(declare-fun v0 () Int)
(declare-fun v1 () Int)
(assert (let ((e2 0))
(let ((e3 2))
(let ((e4 0))
(let ((e5 (- v0 v0)))
(let ((e6 (* (- e2) e5)))
(let ((e7 (- v0)))
(let ((e8 (- e6 e5)))
(let ((e9 (* e2 e7)))
(let ((e10 (- e6)))
(let ((e11 (* v0 (- e3))))
(let ((e12 (+ e8 e10)))
(let ((e13 (+ e5 e10)))
(let ((e14 (- e13 e6)))
(let ((e15 (- e12)))
(let ((e16 (+ e6 e14)))
(let ((e17 (* e14 e4)))
(let ((e18 (- e16)))
(let ((e19 (* (- e2) v1)))
(let ((e20 (>= e17 e6)))
(let ((e21 (> e9 e5)))
(let ((e22 (<= e7 e19)))
(let ((e23 (= e13 e7)))
(let ((e24 (> e6 e8)))
(let ((e25 (>= e8 e9)))
(let ((e26 (> e8 e19)))
(let ((e27 (< v0 e5)))
(let ((e28 (>= e8 v1)))
(let ((e29 (< e10 v0)))
(let ((e30 (= e19 e5)))
(let ((e31 (< e15 e6)))
(let ((e32 (distinct e7 e14)))
(let ((e33 (> e13 e13)))
(let ((e34 (<= v1 e18)))
(let ((e35 (> e11 e11)))
(let ((e36 (= e8 e13)))
(let ((e37 (< e18 e9)))
(let ((e38 (<= e12 e11)))
(let ((e39 (= e11 e6)))
(let ((e40 (> e13 e14)))
(let ((e41 (<= e15 e14)))
(let ((e42 (distinct e19 e8)))
(let ((e43 (<= v0 e7)))
(let ((e44 (distinct e9 e17)))
(let ((e45 (= e11 e13)))
(let ((e46 (= e18 e8)))
(let ((e47 (= e8 e9)))
(let ((e48 (< e10 e14)))
(let ((e49 (= e9 e12)))
(let ((e50 (= v0 e13)))
(let ((e51 (>= e16 e7)))
(let ((e52 (ite e28 e16 e19)))
(let ((e53 (ite e29 e13 e15)))
(let ((e54 (ite e20 e16 e53)))
(let ((e55 (ite e45 e5 e7)))
(let ((e56 (ite e36 e55 e12)))
(let ((e57 (ite e34 e9 e5)))
(let ((e58 (ite e24 e15 e54)))
(let ((e59 (ite e44 v0 e8)))
(let ((e60 (ite e26 e10 e19)))
(let ((e61 (ite e43 e14 e8)))
(let ((e62 (ite e51 e18 e9)))
(let ((e63 (ite e25 e52 e10)))
(let ((e64 (ite e23 e60 e56)))
(let ((e65 (ite e50 e6 v0)))
(let ((e66 (ite e50 e62 e59)))
(let ((e67 (ite e40 e66 e19)))
(let ((e68 (ite e22 e17 e13)))
(let ((e69 (ite e45 v1 e53)))
(let ((e70 (ite e39 e18 e16)))
(let ((e71 (ite e30 e62 e65)))
(let ((e72 (ite e35 e11 e54)))
(let ((e73 (ite e41 e60 e7)))
(let ((e74 (ite e47 e57 e57)))
(let ((e75 (ite e32 e53 e62)))
(let ((e76 (ite e27 v1 e7)))
(let ((e77 (ite e43 e53 e57)))
(let ((e78 (ite e42 e59 e5)))
(let ((e79 (ite e33 e5 e53)))
(let ((e80 (ite e21 e11 e63)))
(let ((e81 (ite e38 e6 e60)))
(let ((e82 (ite e25 e65 e77)))
(let ((e83 (ite e22 v0 e6)))
(let ((e84 (ite e49 e5 e11)))
(let ((e85 (ite e48 e61 e82)))
(let ((e86 (ite e21 e65 e19)))
(let ((e87 (ite e49 e12 e61)))
(let ((e88 (ite e20 e61 e70)))
(let ((e89 (ite e37 e72 e79)))
(let ((e90 (ite e46 e18 e78)))
(let ((e91 (ite e36 e63 e15)))
(let ((e92 (ite e31 e16 v1)))
(let ((e93 (>= e84 e92)))
(let ((e94 (>= e89 e10)))
(let ((e95 (= e7 e55)))
(let ((e96 (> e19 e76)))
(let ((e97 (> e16 e63)))
(let ((e98 (>= e78 e61)))
(let ((e99 (distinct e55 e87)))
(let ((e100 (> e13 e75)))
(let ((e101 (<= e84 e79)))
(let ((e102 (<= e70 e59)))
(let ((e103 (< e79 e19)))
(let ((e104 (< e78 e79)))
(let ((e105 (= e75 e13)))
(let ((e106 (>= e75 e17)))
(let ((e107 (= e78 e18)))
(let ((e108 (>= e83 e57)))
(let ((e109 (= e73 e10)))
(let ((e110 (>= e86 e57)))
(let ((e111 (>= e81 e14)))
(let ((e112 (distinct e5 e8)))
(let ((e113 (<= e72 e91)))
(let ((e114 (>= e71 e89)))
(let ((e115 (< e87 e90)))
(let ((e116 (>= e74 e82)))
(let ((e117 (distinct v1 e64)))
(let ((e118 (<= e52 e14)))
(let ((e119 (>= e75 e8)))
(let ((e120 (<= e57 e82)))
(let ((e121 (<= e90 e73)))
(let ((e122 (< e80 e57)))
(let ((e123 (< e12 e71)))
(let ((e124 (< e6 e68)))
(let ((e125 (distinct e64 e61)))
(let ((e126 (distinct e66 e78)))
(let ((e127 (> e76 e13)))
(let ((e128 (distinct e66 e12)))
(let ((e129 (>= e53 e74)))
(let ((e130 (>= e17 e57)))
(let ((e131 (distinct e64 e77)))
(let ((e132 (distinct e63 e83)))
(let ((e133 (>= e67 e78)))
(let ((e134 (> e78 e92)))
(let ((e135 (<= e77 e8)))
(let ((e136 (> e16 e69)))
(let ((e137 (> e69 e66)))
(let ((e138 (<= e6 e61)))
(let ((e139 (<= e57 e54)))
(let ((e140 (= e66 e79)))
(let ((e141 (= e53 e59)))
(let ((e142 (< e56 e69)))
(let ((e143 (< e69 e77)))
(let ((e144 (<= e86 e89)))
(let ((e145 (<= e73 e75)))
(let ((e146 (<= e14 e7)))
(let ((e147 (> e19 e72)))
(let ((e148 (> e82 e15)))
(let ((e149 (= e81 e5)))
(let ((e150 (<= e88 e18)))
(let ((e151 (<= e10 v1)))
(let ((e152 (<= e83 e53)))
(let ((e153 (>= e73 e89)))
(let ((e154 (> v0 e91)))
(let ((e155 (>= e8 v0)))
(let ((e156 (<= v1 e8)))
(let ((e157 (> e19 e66)))
(let ((e158 (<= e92 e8)))
(let ((e159 (> e70 e92)))
(let ((e160 (< e83 e78)))
(let ((e161 (> e7 e9)))
(let ((e162 (> e54 e88)))
(let ((e163 (< e53 e9)))
(let ((e164 (>= e65 e55)))
(let ((e165 (< e70 e8)))
(let ((e166 (= e71 e18)))
(let ((e167 (= e15 e81)))
(let ((e168 (distinct e68 e85)))
(let ((e169 (distinct e16 e56)))
(let ((e170 (>= e18 e52)))
(let ((e171 (> e85 e18)))
(let ((e172 (>= e52 e87)))
(let ((e173 (distinct e19 e70)))
(let ((e174 (< e90 e92)))
(let ((e175 (> e64 e73)))
(let ((e176 (>= e12 e88)))
(let ((e177 (distinct v0 e67)))
(let ((e178 (= e19 e59)))
(let ((e179 (= e65 e14)))
(let ((e180 (> e19 e13)))
(let ((e181 (< e54 e92)))
(let ((e182 (distinct e57 e68)))
(let ((e183 (> e82 e64)))
(let ((e184 (< e17 e76)))
(let ((e185 (= v0 e55)))
(let ((e186 (distinct e75 e64)))
(let ((e187 (< e71 e56)))
(let ((e188 (= e18 e77)))
(let ((e189 (> e10 e80)))
(let ((e190 (> e82 e70)))
(let ((e191 (>= e62 e82)))
(let ((e192 (> e78 e61)))
(let ((e193 (<= e72 e92)))
(let ((e194 (< e52 e64)))
(let ((e195 (= e76 e7)))
(let ((e196 (<= e70 v0)))
(let ((e197 (<= e58 e91)))
(let ((e198 (distinct e61 e58)))
(let ((e199 (> e64 e71)))
(let ((e200 (<= e10 e58)))
(let ((e201 (>= e66 e92)))
(let ((e202 (<= e91 e70)))
(let ((e203 (<= e60 v1)))
(let ((e204 (distinct e82 e62)))
(let ((e205 (> e65 e16)))
(let ((e206 (= e53 e11)))
(let ((e207 (= e99 e147)))
(let ((e208 (or e27 e152)))
(let ((e209 (= e189 e170)))
(let ((e210 (or e124 e43)))
(let ((e211 (and e157 e25)))
(let ((e212 (and e115 e37)))
(let ((e213 (not e96)))
(let ((e214 (ite e141 e47 e149)))
(let ((e215 (not e140)))
(let ((e216 (xor e95 e177)))
(let ((e217 (xor e178 e169)))
(let ((e218 (= e201 e26)))
(let ((e219 (ite e168 e133 e176)))
(let ((e220 (and e132 e204)))
(let ((e221 (xor e103 e134)))
(let ((e222 (and e171 e34)))
(let ((e223 (and e126 e46)))
(let ((e224 (=> e220 e212)))
(let ((e225 (and e108 e131)))
(let ((e226 (and e105 e218)))
(let ((e227 (= e207 e107)))
(let ((e228 (and e123 e123)))
(let ((e229 (=> e185 e44)))
(let ((e230 (or e113 e167)))
(let ((e231 (and e219 e29)))
(let ((e232 (= e200 e192)))
(let ((e233 (not e181)))
(let ((e234 (= e227 e223)))
(let ((e235 (or e42 e112)))
(let ((e236 (not e136)))
(let ((e237 (not e193)))
(let ((e238 (or e158 e144)))
(let ((e239 (=> e118 e101)))
(let ((e240 (and e98 e216)))
(let ((e241 (or e146 e231)))
(let ((e242 (not e51)))
(let ((e243 (=> e242 e50)))
(let ((e244 (not e206)))
(let ((e245 (or e163 e33)))
(let ((e246 (and e117 e41)))
(let ((e247 (=> e106 e237)))
(let ((e248 (=> e180 e36)))
(let ((e249 (ite e138 e172 e137)))
(let ((e250 (=> e247 e94)))
(let ((e251 (xor e174 e122)))
(let ((e252 (or e248 e100)))
(let ((e253 (and e31 e23)))
(let ((e254 (= e233 e114)))
(let ((e255 (ite e49 e22 e110)))
(let ((e256 (= e222 e142)))
(let ((e257 (and e210 e127)))
(let ((e258 (ite e195 e217 e196)))
(let ((e259 (=> e150 e238)))
(let ((e260 (= e184 e164)))
(let ((e261 (ite e213 e143 e188)))
(let ((e262 (and e214 e256)))
(let ((e263 (=> e28 e97)))
(let ((e264 (and e194 e225)))
(let ((e265 (or e263 e252)))
(let ((e266 (xor e119 e262)))
(let ((e267 (xor e109 e198)))
(let ((e268 (not e183)))
(let ((e269 (ite e229 e251 e32)))
(let ((e270 (or e155 e253)))
(let ((e271 (xor e270 e187)))
(let ((e272 (xor e241 e116)))
(let ((e273 (and e20 e93)))
(let ((e274 (or e267 e128)))
(let ((e275 (and e166 e228)))
(let ((e276 (=> e244 e156)))
(let ((e277 (xor e154 e21)))
(let ((e278 (xor e205 e203)))
(let ((e279 (or e245 e130)))
(let ((e280 (ite e202 e39 e202)))
(let ((e281 (not e191)))
(let ((e282 (xor e135 e45)))
(let ((e283 (not e277)))
(let ((e284 (ite e243 e281 e273)))
(let ((e285 (not e246)))
(let ((e286 (= e258 e182)))
(let ((e287 (not e215)))
(let ((e288 (= e287 e120)))
(let ((e289 (not e283)))
(let ((e290 (=> e209 e40)))
(let ((e291 (and e239 e24)))
(let ((e292 (and e264 e284)))
(let ((e293 (or e165 e30)))
(let ((e294 (= e102 e278)))
(let ((e295 (and e289 e269)))
(let ((e296 (ite e175 e240 e226)))
(let ((e297 (not e266)))
(let ((e298 (xor e125 e232)))
(let ((e299 (not e254)))
(let ((e300 (not e159)))
(let ((e301 (ite e274 e255 e35)))
(let ((e302 (xor e234 e230)))
(let ((e303 (xor e259 e179)))
(let ((e304 (and e235 e173)))
(let ((e305 (xor e299 e271)))
(let ((e306 (not e300)))
(let ((e307 (ite e296 e290 e257)))
(let ((e308 (not e303)))
(let ((e309 (not e280)))
(let ((e310 (xor e309 e148)))
(let ((e311 (ite e293 e249 e306)))
(let ((e312 (ite e121 e224 e160)))
(let ((e313 (or e297 e129)))
(let ((e314 (and e268 e305)))
(let ((e315 (=> e295 e236)))
(let ((e316 (ite e48 e151 e286)))
(let ((e317 (and e301 e292)))
(let ((e318 (and e162 e291)))
(let ((e319 (= e261 e197)))
(let ((e320 (or e279 e186)))
(let ((e321 (or e312 e314)))
(let ((e322 (=> e304 e190)))
(let ((e323 (ite e276 e250 e139)))
(let ((e324 (= e316 e316)))
(let ((e325 (ite e317 e211 e199)))
(let ((e326 (and e315 e319)))
(let ((e327 (=> e272 e104)))
(let ((e328 (ite e318 e323 e325)))
(let ((e329 (ite e288 e324 e308)))
(let ((e330 (= e221 e328)))
(let ((e331 (not e311)))
(let ((e332 (or e111 e161)))
(let ((e333 (=> e275 e307)))
(let ((e334 (xor e145 e282)))
(let ((e335 (not e333)))
(let ((e336 (or e38 e330)))
(let ((e337 (or e313 e320)))
(let ((e338 (and e208 e260)))
(let ((e339 (xor e336 e335)))
(let ((e340 (=> e310 e329)))
(let ((e341 (= e334 e265)))
(let ((e342 (=> e327 e338)))
(let ((e343 (ite e322 e298 e326)))
(let ((e344 (and e339 e337)))
(let ((e345 (or e294 e294)))
(let ((e346 (and e302 e341)))
(let ((e347 (ite e340 e346 e332)))
(let ((e348 (=> e344 e331)))
(let ((e349 (not e321)))
(let ((e350 (or e285 e345)))
(let ((e351 (not e350)))
(let ((e352 (xor e348 e351)))
(let ((e353 (or e352 e352)))
(let ((e354 (or e353 e342)))
(let ((e355 (or e347 e347)))
(let ((e356 (not e153)))
(let ((e357 (ite e354 e355 e355)))
(let ((e358 (or e356 e356)))
(let ((e359 (and e349 e357)))
(let ((e360 (ite e359 e343 e358)))
e360
))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(check-sat)
