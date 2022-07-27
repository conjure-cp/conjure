(set-info :source |fuzzsmt|)
(set-info :smt-lib-version 2.0)
(set-info :category "random")
(set-info :status unknown)
(set-logic QF_UF)
(declare-sort S0 0)
(declare-sort S1 0)
(declare-fun v0 () S0)
(declare-fun v1 () S0)
(declare-fun v2 () S0)
(declare-fun v3 () S1)
(declare-fun v4 () S1)
(declare-fun v5 () S1)
(declare-fun f0 ( S0 S0 S1) S1)
(declare-fun f1 ( S1 S0 S0) S0)
(declare-fun f2 ( S1) S0)
(declare-fun f3 ( S1) S0)
(declare-fun f4 ( S0) S0)
(declare-fun p0 ( S0 S1 S1) Bool)
(declare-fun p1 ( S1 S1 S1) Bool)
(declare-fun p2 ( S1 S0 S0) Bool)
(declare-fun p3 ( S1 S0 S1) Bool)
(declare-fun p4 ( S0 S0) Bool)
(assert (let ((e6 (f3 v4)))
(let ((e7 (! (f4 v2) :named term7)))
(let ((e8 (! (f3 v5) :named term8)))
(let ((e9 (! (f4 v0) :named term9)))
(let ((e10 (! (f4 v1) :named term10)))
(let ((e11 (! (f2 v5) :named term11)))
(let ((e12 (! (f0 v1 v0 v3) :named term12)))
(let ((e13 (! (f3 e12) :named term13)))
(let ((e14 (! (f4 e9) :named term14)))
(let ((e15 (! (f3 v5) :named term15)))
(let ((e16 (! (f1 e12 e6 e6) :named term16)))
(let ((e17 (! (p3 v4 e15 v3) :named term17)))
(let ((e18 (! (= e12 v5) :named term18)))
(let ((e19 (! (p2 v3 e7 e14) :named term19)))
(let ((e20 (! (p0 e16 v4 v3) :named term20)))
(let ((e21 (! (p2 v4 v1 e16) :named term21)))
(let ((e22 (! (p2 v3 e13 v1) :named term22)))
(let ((e23 (! (= e9 e6) :named term23)))
(let ((e24 (! (p3 v5 e16 v4) :named term24)))
(let ((e25 (! (p1 v4 v4 v3) :named term25)))
(let ((e26 (! (distinct v2 e15) :named term26)))
(let ((e27 (! (p2 e12 e16 e15) :named term27)))
(let ((e28 (! (p3 v5 e14 v4) :named term28)))
(let ((e29 (! (distinct e11 e7) :named term29)))
(let ((e30 (! (distinct v0 e15) :named term30)))
(let ((e31 (! (p2 e12 e14 e11) :named term31)))
(let ((e32 (! (distinct e8 e9) :named term32)))
(let ((e33 (! (p1 v3 v3 v4) :named term33)))
(let ((e34 (! (p2 v5 e11 e9) :named term34)))
(let ((e35 (! (p1 e12 v5 v4) :named term35)))
(let ((e36 (! (distinct e10 e11) :named term36)))
(let ((e37 (! (p3 v4 e7 v5) :named term37)))
(let ((e38 (! (p0 e8 v4 v5) :named term38)))
(let ((e39 (! (p2 v5 e8 e8) :named term39)))
(let ((e40 (! (p0 v0 v4 v3) :named term40)))
(let ((e41 (! (p4 e15 e13) :named term41)))
(let ((e42 (! (ite e22 v5 v3) :named term42)))
(let ((e43 (! (ite e29 e9 e7) :named term43)))
(let ((e44 (! (ite e33 e8 v0) :named term44)))
(let ((e45 (! (ite e40 e12 e12) :named term45)))
(let ((e46 (! (ite e41 e13 e14) :named term46)))
(let ((e47 (! (ite e29 v1 v1) :named term47)))
(let ((e48 (! (ite e23 e16 v2) :named term48)))
(let ((e49 (! (ite e29 e15 e9) :named term49)))
(let ((e50 (! (ite e32 e49 v2) :named term50)))
(let ((e51 (! (ite e35 e11 e13) :named term51)))
(let ((e52 (! (ite e30 v3 e12) :named term52)))
(let ((e53 (! (ite e18 v4 e12) :named term53)))
(let ((e54 (! (ite e35 v5 e45) :named term54)))
(let ((e55 (! (ite e21 e7 e13) :named term55)))
(let ((e56 (! (ite e19 e48 v2) :named term56)))
(let ((e57 (! (ite e19 e6 e11) :named term57)))
(let ((e58 (! (ite e17 e55 e7) :named term58)))
(let ((e59 (! (ite e22 e56 e8) :named term59)))
(let ((e60 (! (ite e38 e12 v5) :named term60)))
(let ((e61 (! (ite e36 e43 e57) :named term61)))
(let ((e62 (! (ite e26 e10 e9) :named term62)))
(let ((e63 (! (ite e32 v1 e56) :named term63)))
(let ((e64 (! (ite e35 e58 v1) :named term64)))
(let ((e65 (! (ite e22 e13 e48) :named term65)))
(let ((e66 (! (ite e27 e57 e44) :named term66)))
(let ((e67 (! (ite e27 e12 e42) :named term67)))
(let ((e68 (! (ite e21 e8 e47) :named term68)))
(let ((e69 (! (ite e33 e43 e65) :named term69)))
(let ((e70 (! (ite e31 v3 e42) :named term70)))
(let ((e71 (! (ite e41 e6 e6) :named term71)))
(let ((e72 (! (ite e25 e58 e50) :named term72)))
(let ((e73 (! (ite e24 e56 e9) :named term73)))
(let ((e74 (! (ite e33 e51 e7) :named term74)))
(let ((e75 (! (ite e22 e58 v2) :named term75)))
(let ((e76 (! (ite e30 e67 e60) :named term76)))
(let ((e77 (! (ite e20 e68 e43) :named term77)))
(let ((e78 (! (ite e34 e56 e9) :named term78)))
(let ((e79 (! (ite e39 v4 e70) :named term79)))
(let ((e80 (! (ite e38 e53 e12) :named term80)))
(let ((e81 (! (ite e38 e8 e43) :named term81)))
(let ((e82 (! (ite e28 e64 e8) :named term82)))
(let ((e83 (! (ite e37 e79 e54) :named term83)))
(let ((e84 (! (p0 e61 e76 e12) :named term84)))
(let ((e85 (! (distinct e58 e47) :named term85)))
(let ((e86 (! (distinct e42 e52) :named term86)))
(let ((e87 (! (p3 e12 v0 e67) :named term87)))
(let ((e88 (! (= e71 e66) :named term88)))
(let ((e89 (! (distinct e8 e51) :named term89)))
(let ((e90 (! (distinct e72 e68) :named term90)))
(let ((e91 (! (p0 v2 e12 e12) :named term91)))
(let ((e92 (! (= e82 e55) :named term92)))
(let ((e93 (! (p2 e80 e78 e58) :named term93)))
(let ((e94 (! (p1 e70 v3 e76) :named term94)))
(let ((e95 (! (p0 v1 v3 e60) :named term95)))
(let ((e96 (! (p4 e57 e55) :named term96)))
(let ((e97 (! (= e83 e83) :named term97)))
(let ((e98 (! (p4 e81 e72) :named term98)))
(let ((e99 (! (= e54 e83) :named term99)))
(let ((e100 (! (p3 e83 e64 e76) :named term100)))
(let ((e101 (! (p3 e42 e49 e53) :named term101)))
(let ((e102 (! (p2 e79 e74 e14) :named term102)))
(let ((e103 (! (= e44 e78) :named term103)))
(let ((e104 (! (p1 e80 e80 v5) :named term104)))
(let ((e105 (! (p2 v3 v0 e44) :named term105)))
(let ((e106 (! (distinct e7 v0) :named term106)))
(let ((e107 (! (= e56 e81) :named term107)))
(let ((e108 (! (p1 e76 e67 e67) :named term108)))
(let ((e109 (! (p2 e54 e69 e14) :named term109)))
(let ((e110 (! (distinct e9 e15) :named term110)))
(let ((e111 (! (= e59 e10) :named term111)))
(let ((e112 (! (= e62 e14) :named term112)))
(let ((e113 (! (distinct e13 e62) :named term113)))
(let ((e114 (! (distinct e43 e8) :named term114)))
(let ((e115 (! (= e16 e6) :named term115)))
(let ((e116 (! (distinct e46 e72) :named term116)))
(let ((e117 (! (= v4 e52) :named term117)))
(let ((e118 (! (p1 e83 e70 e42) :named term118)))
(let ((e119 (! (= e65 e58) :named term119)))
(let ((e120 (! (p0 e69 e67 e83) :named term120)))
(let ((e121 (! (= e45 e60) :named term121)))
(let ((e122 (! (p1 v4 v4 e76) :named term122)))
(let ((e123 (! (p3 e76 e72 e54) :named term123)))
(let ((e124 (! (p4 e43 e73) :named term124)))
(let ((e125 (! (distinct e63 e16) :named term125)))
(let ((e126 (! (p2 e12 e57 e74) :named term126)))
(let ((e127 (! (distinct e50 e55) :named term127)))
(let ((e128 (! (= e77 e46) :named term128)))
(let ((e129 (! (p3 e45 e6 e42) :named term129)))
(let ((e130 (! (p2 e45 e50 e9) :named term130)))
(let ((e131 (! (p4 e44 e81) :named term131)))
(let ((e132 (! (p0 e11 e79 e79) :named term132)))
(let ((e133 (! (p0 e61 e45 e67) :named term133)))
(let ((e134 (! (p3 e76 e11 v4) :named term134)))
(let ((e135 (! (p4 e55 e46) :named term135)))
(let ((e136 (! (p1 v4 e52 e83) :named term136)))
(let ((e137 (! (p4 e72 v2) :named term137)))
(let ((e138 (! (p3 v5 v1 e76) :named term138)))
(let ((e139 (! (p4 v0 e7) :named term139)))
(let ((e140 (! (p2 e76 e55 e51) :named term140)))
(let ((e141 (! (p4 v2 v0) :named term141)))
(let ((e142 (! (distinct e48 v0) :named term142)))
(let ((e143 (! (= e75 e8) :named term143)))
(let ((e144 (! (= e22 e87) :named term144)))
(let ((e145 (! (and e31 e112) :named term145)))
(let ((e146 (! (or e91 e134) :named term146)))
(let ((e147 (! (not e137) :named term147)))
(let ((e148 (! (ite e26 e131 e114) :named term148)))
(let ((e149 (! (not e102) :named term149)))
(let ((e150 (! (or e30 e127) :named term150)))
(let ((e151 (! (xor e115 e128) :named term151)))
(let ((e152 (! (= e100 e143) :named term152)))
(let ((e153 (! (ite e121 e38 e142) :named term153)))
(let ((e154 (! (or e32 e29) :named term154)))
(let ((e155 (! (not e41) :named term155)))
(let ((e156 (! (and e109 e145) :named term156)))
(let ((e157 (! (= e139 e99) :named term157)))
(let ((e158 (! (xor e156 e24) :named term158)))
(let ((e159 (! (=> e103 e33) :named term159)))
(let ((e160 (! (=> e35 e88) :named term160)))
(let ((e161 (! (or e85 e125) :named term161)))
(let ((e162 (! (and e160 e157) :named term162)))
(let ((e163 (! (or e129 e123) :named term163)))
(let ((e164 (! (ite e106 e151 e132) :named term164)))
(let ((e165 (! (= e146 e124) :named term165)))
(let ((e166 (! (or e108 e119) :named term166)))
(let ((e167 (! (ite e36 e17 e40) :named term167)))
(let ((e168 (! (not e37) :named term168)))
(let ((e169 (! (ite e158 e19 e94) :named term169)))
(let ((e170 (! (and e161 e150) :named term170)))
(let ((e171 (! (and e39 e92) :named term171)))
(let ((e172 (! (xor e135 e20) :named term172)))
(let ((e173 (! (=> e148 e116) :named term173)))
(let ((e174 (! (not e110) :named term174)))
(let ((e175 (! (and e170 e113) :named term175)))
(let ((e176 (! (= e117 e152) :named term176)))
(let ((e177 (! (not e169) :named term177)))
(let ((e178 (! (or e167 e93) :named term178)))
(let ((e179 (! (or e28 e147) :named term179)))
(let ((e180 (! (=> e163 e141) :named term180)))
(let ((e181 (! (or e174 e174) :named term181)))
(let ((e182 (! (and e176 e89) :named term182)))
(let ((e183 (! (xor e164 e105) :named term183)))
(let ((e184 (! (or e86 e159) :named term184)))
(let ((e185 (! (and e18 e183) :named term185)))
(let ((e186 (! (xor e140 e182) :named term186)))
(let ((e187 (! (=> e120 e149) :named term187)))
(let ((e188 (! (and e95 e171) :named term188)))
(let ((e189 (! (and e187 e178) :named term189)))
(let ((e190 (! (xor e179 e107) :named term190)))
(let ((e191 (! (ite e172 e27 e168) :named term191)))
(let ((e192 (! (and e173 e126) :named term192)))
(let ((e193 (! (and e144 e23) :named term193)))
(let ((e194 (! (xor e34 e122) :named term194)))
(let ((e195 (! (= e180 e97) :named term195)))
(let ((e196 (! (xor e190 e138) :named term196)))
(let ((e197 (! (ite e153 e133 e84) :named term197)))
(let ((e198 (! (xor e194 e175) :named term198)))
(let ((e199 (! (xor e188 e130) :named term199)))
(let ((e200 (! (not e165) :named term200)))
(let ((e201 (! (and e193 e25) :named term201)))
(let ((e202 (! (xor e166 e98) :named term202)))
(let ((e203 (! (xor e186 e184) :named term203)))
(let ((e204 (! (xor e192 e96) :named term204)))
(let ((e205 (! (ite e111 e200 e90) :named term205)))
(let ((e206 (! (=> e202 e191) :named term206)))
(let ((e207 (! (and e201 e155) :named term207)))
(let ((e208 (! (=> e177 e185) :named term208)))
(let ((e209 (! (xor e104 e196) :named term209)))
(let ((e210 (! (not e197) :named term210)))
(let ((e211 (! (= e206 e208) :named term211)))
(let ((e212 (! (=> e21 e101) :named term212)))
(let ((e213 (! (and e198 e211) :named term213)))
(let ((e214 (! (= e181 e154) :named term214)))
(let ((e215 (! (and e195 e210) :named term215)))
(let ((e216 (! (xor e199 e213) :named term216)))
(let ((e217 (! (ite e214 e189 e162) :named term217)))
(let ((e218 (! (and e136 e217) :named term218)))
(let ((e219 (! (ite e205 e218 e203) :named term219)))
(let ((e220 (! (xor e215 e219) :named term220)))
(let ((e221 (! (=> e212 e220) :named term221)))
(let ((e222 (! (ite e209 e207 e207) :named term222)))
(let ((e223 (! (=> e204 e222) :named term223)))
(let ((e224 (! (and e216 e221) :named term224)))
(let ((e225 (! (not e118) :named term225)))
(let ((e226 (! (= e225 e224) :named term226)))
(let ((e227 (! (=> e223 e226) :named term227)))
e227
)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(check-sat)
(set-option :regular-output-channel "/dev/null")
(get-model)
(get-value (term7))
(get-value (term8))
(get-value (term9))
(get-value (term10))
(get-value (term11))
(get-value (term12))
(get-value (term13))
(get-value (term14))
(get-value (term15))
(get-value (term16))
(get-value (term17))
(get-value (term18))
(get-value (term19))
(get-value (term20))
(get-value (term21))
(get-value (term22))
(get-value (term23))
(get-value (term24))
(get-value (term25))
(get-value (term26))
(get-value (term27))
(get-value (term28))
(get-value (term29))
(get-value (term30))
(get-value (term31))
(get-value (term32))
(get-value (term33))
(get-value (term34))
(get-value (term35))
(get-value (term36))
(get-value (term37))
(get-value (term38))
(get-value (term39))
(get-value (term40))
(get-value (term41))
(get-value (term42))
(get-value (term43))
(get-value (term44))
(get-value (term45))
(get-value (term46))
(get-value (term47))
(get-value (term48))
(get-value (term49))
(get-value (term50))
(get-value (term51))
(get-value (term52))
(get-value (term53))
(get-value (term54))
(get-value (term55))
(get-value (term56))
(get-value (term57))
(get-value (term58))
(get-value (term59))
(get-value (term60))
(get-value (term61))
(get-value (term62))
(get-value (term63))
(get-value (term64))
(get-value (term65))
(get-value (term66))
(get-value (term67))
(get-value (term68))
(get-value (term69))
(get-value (term70))
(get-value (term71))
(get-value (term72))
(get-value (term73))
(get-value (term74))
(get-value (term75))
(get-value (term76))
(get-value (term77))
(get-value (term78))
(get-value (term79))
(get-value (term80))
(get-value (term81))
(get-value (term82))
(get-value (term83))
(get-value (term84))
(get-value (term85))
(get-value (term86))
(get-value (term87))
(get-value (term88))
(get-value (term89))
(get-value (term90))
(get-value (term91))
(get-value (term92))
(get-value (term93))
(get-value (term94))
(get-value (term95))
(get-value (term96))
(get-value (term97))
(get-value (term98))
(get-value (term99))
(get-value (term100))
(get-value (term101))
(get-value (term102))
(get-value (term103))
(get-value (term104))
(get-value (term105))
(get-value (term106))
(get-value (term107))
(get-value (term108))
(get-value (term109))
(get-value (term110))
(get-value (term111))
(get-value (term112))
(get-value (term113))
(get-value (term114))
(get-value (term115))
(get-value (term116))
(get-value (term117))
(get-value (term118))
(get-value (term119))
(get-value (term120))
(get-value (term121))
(get-value (term122))
(get-value (term123))
(get-value (term124))
(get-value (term125))
(get-value (term126))
(get-value (term127))
(get-value (term128))
(get-value (term129))
(get-value (term130))
(get-value (term131))
(get-value (term132))
(get-value (term133))
(get-value (term134))
(get-value (term135))
(get-value (term136))
(get-value (term137))
(get-value (term138))
(get-value (term139))
(get-value (term140))
(get-value (term141))
(get-value (term142))
(get-value (term143))
(get-value (term144))
(get-value (term145))
(get-value (term146))
(get-value (term147))
(get-value (term148))
(get-value (term149))
(get-value (term150))
(get-value (term151))
(get-value (term152))
(get-value (term153))
(get-value (term154))
(get-value (term155))
(get-value (term156))
(get-value (term157))
(get-value (term158))
(get-value (term159))
(get-value (term160))
(get-value (term161))
(get-value (term162))
(get-value (term163))
(get-value (term164))
(get-value (term165))
(get-value (term166))
(get-value (term167))
(get-value (term168))
(get-value (term169))
(get-value (term170))
(get-value (term171))
(get-value (term172))
(get-value (term173))
(get-value (term174))
(get-value (term175))
(get-value (term176))
(get-value (term177))
(get-value (term178))
(get-value (term179))
(get-value (term180))
(get-value (term181))
(get-value (term182))
(get-value (term183))
(get-value (term184))
(get-value (term185))
(get-value (term186))
(get-value (term187))
(get-value (term188))
(get-value (term189))
(get-value (term190))
(get-value (term191))
(get-value (term192))
(get-value (term193))
(get-value (term194))
(get-value (term195))
(get-value (term196))
(get-value (term197))
(get-value (term198))
(get-value (term199))
(get-value (term200))
(get-value (term201))
(get-value (term202))
(get-value (term203))
(get-value (term204))
(get-value (term205))
(get-value (term206))
(get-value (term207))
(get-value (term208))
(get-value (term209))
(get-value (term210))
(get-value (term211))
(get-value (term212))
(get-value (term213))
(get-value (term214))
(get-value (term215))
(get-value (term216))
(get-value (term217))
(get-value (term218))
(get-value (term219))
(get-value (term220))
(get-value (term221))
(get-value (term222))
(get-value (term223))
(get-value (term224))
(get-value (term225))
(get-value (term226))
(get-value (term227))
(get-info :all-statistics)
