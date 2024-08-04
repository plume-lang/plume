[native function native.plmc.to_string 1,native function native.plmc.copy_ref 1,native function native.plmc.free_ref 1,function show_prec($inst) {
  return $inst.index(LInt 0)
},function @lambda0(@lambda0_env, a) {
  let var0 = @lambda0_env.0
  let call0 = show_prec(@lambda0_env.0)
  let var71 = call0.1
  let var72 = var71(call0.0, a, LInt 0)
  return var72
},function show(var0) {
  return {0: {0: var0}, 1: @lambda0}
},function and($inst) {
  return $inst.index(LInt 0)
},function or($inst) {
  return $inst.index(LInt 2)
},function not($inst) {
  return $inst.index(LInt 1)
},function to_str($inst) {
  return $inst.index(LInt 0)
},function from_str($inst) {
  return $inst.index(LInt 0)
},function from($inst) {
  return $inst.index(LInt 0)
},function to($inst) {
  return $inst.index(LInt 1)
},function ==($inst) {
  return $inst.index(LInt 0)
},function +($inst) {
  return $inst.index(LInt 0)
},function -($inst) {
  return $inst.index(LInt 1)
},function *($inst) {
  return $inst.index(LInt 0)
},function /($inst) {
  return $inst.index(LInt 1)
},function ^($inst) {
  return $inst.index(LInt 2)
},function <($inst) {
  return $inst.index(LInt 0)
},function @lambda1(@lambda1_env, a, b) {
  let var1 = @lambda1_env.0
  let var2 = @lambda1_env.1
  let var3 = @lambda1_env.2
  let call3 = or(@lambda1_env.0)
  let var77 = call3.1
  let call1 = <(@lambda1_env.1)
  let var73 = call1.1
  let var74 = var73(call1.0, a, b)
  let call2 = ==(@lambda1_env.2)
  let var75 = call2.1
  let var76 = var75(call2.0, a, b)
  let var78 = var77(call3.0, var74, var76)
  return var78
},function <=(var1, var2, var3) {
  return {0: {0: var1, 1: var2, 2: var3}, 1: @lambda1}
},function @lambda2(@lambda2_env, a, b) {
  let var4 = @lambda2_env.0
  let var5 = @lambda2_env.1
  let call5 = not(@lambda2_env.0)
  let var81 = call5.1
  let call4 = <(@lambda2_env.1)
  let var79 = call4.1
  let var80 = var79(call4.0, a, b)
  let var82 = var81(call5.0, var80)
  return var82
},function >=(var4, var5) {
  return {0: {0: var4, 1: var5}, 1: @lambda2}
},function @lambda3(@lambda3_env, a, b) {
  let var7 = @lambda3_env.0
  let var8 = @lambda3_env.1
  let var9 = @lambda3_env.2
  let call7 = not(@lambda3_env.0)
  let var85 = call7.1
  let call6 = <=(@lambda3_env.0, @lambda3_env.1, @lambda3_env.2)
  let var83 = call6.1
  let var84 = var83(call6.0, a, b)
  let var86 = var85(call7.0, var84)
  return var86
},function >(var7, var8, var9) {
  return {0: {0: var7, 1: var8, 2: var9}, 1: @lambda3}
},function map($inst) {
  return $inst.index(LInt 0)
},function foldl($inst) {
  return $inst.index(LInt 0)
},function get_index($inst) {
  return $inst.index(LInt 0)
},function default($inst) {
  return $inst.index(LInt 0)
},function bind($inst) {
  return $inst.index(LInt 0)
},function pure($inst) {
  return $inst.index(LInt 1)
},function @lambda4(@lambda4_env, x, f) {
  let var10 = @lambda4_env.0
  let call8 = bind(@lambda4_env.0)
  let var87 = call8.1
  let var88 = var87(call8.0, x, f)
  return var88
},function >>=(var10) {
  return {0: {0: var10}, 1: @lambda4}
},function @lambda5(@lambda5_env, _) {
  let y = @lambda5_env.0
  return @lambda5_env.0
},function @lambda6(@lambda6_env, x, y) {
  let @lambda5 = @lambda6_env.0
  let var11 = @lambda6_env.1
  let call9 = bind(@lambda6_env.1)
  let var89 = call9.1
  let var90 = var89(call9.0, x, {0: {0: y}, 1: @lambda6_env.0})
  return var90
},function and_then(var11) {
  return {0: {0: @lambda5, 1: var11}, 1: @lambda6}
},function @lambda7(@lambda7_env, x, y) {
  let var12 = @lambda7_env.0
  let call10 = and_then(@lambda7_env.0)
  let var91 = call10.1
  let var92 = var91(call10.0, x, y)
  return var92
},function >>(var12) {
  return {0: {0: var12}, 1: @lambda7}
},function @lambda8(@lambda8_env, _) {
  let a = @lambda8_env.0
  let var15 = @lambda8_env.1
  let call11 = pure(@lambda8_env.1)
  let var93 = call11.1
  let var94 = var93(call11.0, @lambda8_env.0)
  return var94
},function @lambda9(@lambda9_env, a) {
  let @lambda8 = @lambda9_env.0
  let var15 = @lambda9_env.1
  let y = @lambda9_env.2
  let call12 = bind(@lambda9_env.1)
  let var95 = call12.1
  let var96 = var95(call12.0, @lambda9_env.2, {0: {0: a, 1: @lambda9_env.1}, 1: @lambda9_env.0})
  return var96
},function @lambda10(@lambda10_env, x, y) {
  let @lambda8 = @lambda10_env.0
  let @lambda9 = @lambda10_env.1
  let var15 = @lambda10_env.2
  let call13 = bind(@lambda10_env.2)
  let var97 = call13.1
  let var98 = var97(call13.0, x, {0: {0: @lambda10_env.0, 1: @lambda10_env.2, 2: y}, 1: @lambda10_env.1})
  return var98
},function <<(var15) {
  return {0: {0: @lambda8, 1: @lambda9, 2: var15}, 1: @lambda10}
},function @lambda11(@lambda11_env, var, x) {
  let var16 = @lambda11_env.0
  let call14 = +(@lambda11_env.0)
  let var99 = call14.1
  let var100 = var99(call14.0, unmut var, x)
  UVariable "var" = var100
  return unmut var
},function +=(var16) {
  return {0: {0: var16}, 1: @lambda11}
},function @lambda12(@lambda12_env, var, x) {
  let var17 = @lambda12_env.0
  let call15 = -(@lambda12_env.0)
  let var101 = call15.1
  let var102 = var101(call15.0, unmut var, x)
  UVariable "var" = var102
  return unmut var
},function -=(var17) {
  return {0: {0: var17}, 1: @lambda12}
},function @lambda13(@lambda13_env, var, x) {
  let var18 = @lambda13_env.0
  let call16 = *(@lambda13_env.0)
  let var103 = call16.1
  let var104 = var103(call16.0, unmut var, x)
  UVariable "var" = var104
  return unmut var
},function *=(var18) {
  return {0: {0: var18}, 1: @lambda13}
},function @lambda14(@lambda14_env, var, x) {
  let var19 = @lambda14_env.0
  let call17 = /(@lambda14_env.0)
  let var105 = call17.1
  let var106 = var105(call17.0, unmut var, x)
  UVariable "var" = var106
  return unmut var
},function /=(var19) {
  return {0: {0: var19}, 1: @lambda14}
},function @lambda15(@lambda15_env, x) {
  let var20 = @lambda15_env.0
  let var21 = @lambda15_env.1
  let call19 = -(@lambda15_env.0)
  let var109 = call19.1
  let call18 = default(@lambda15_env.1)
  let var107 = call18.1
  let var108 = var107(call18.0)
  let var110 = var109(call19.0, var108, x)
  return var110
},function negate(var20, var21) {
  return {0: {0: var20, 1: var21}, 1: @lambda15}
},let unit = [special, LString "unit", LString "unit"],function @lambda16(@lambda16_env, _, _) {
  return LString "()"
},let show__unit = [{0: {}, 1: @lambda16}],function @lambda17(@lambda17_env, a, b) {
  return if a then [return b] else [return LBool False]
  return nil
},function @lambda18(@lambda18_env, a) {
  return if a then [return LBool False] else [return LBool True]
  return nil
},function @lambda19(@lambda19_env, a, b) {
  return if a then [return LBool True] else [return b]
  return nil
},let boolean_algebra__bool = [{0: {}, 1: @lambda17}, {0: {}, 1: @lambda18}, {0: {}, 1: @lambda19}],function @lambda20(@lambda20_env, self, _) {
  return if self then [return LString "true"] else [return LString "false"]
  return nil
},let show__bool = [{0: {}, 1: @lambda20}],function @lambda21(@lambda21_env, a, b) {
  let var22 = @lambda21_env.0
  let call21 = not(boolean_algebra__bool)
  let var113 = call21.1
  let call20 = ==(@lambda21_env.0)
  let var111 = call20.1
  let var112 = var111(call20.0, a, b)
  let var114 = var113(call21.0, var112)
  return var114
},function !=(var22) {
  return {0: {0: var22}, 1: @lambda21}
},native function native.plmc.add_str 2,native function native.plmc.mul_str 2,native function native.plmc.string_length 1,native function native.plmc.eq_string 2,native function native.plmc.get_index_str 2,native function native.plmc.str_slice 3,function @lambda22(@lambda22_env, x, y) {
  return add_str(x, y)
},function @lambda23(@lambda23_env, x, y) {
  return x
},let numeric__str = [{0: {}, 1: @lambda22}, {0: {}, 1: @lambda23}],function @lambda24(@lambda24_env, x, y) {
  return eq_string(x, y)
},let equality__str = [{0: {}, 1: @lambda24}],function @lambda25(@lambda25_env, x, prec) {
  return if prec == LInt 0 then [return x] else [if LBool True then [let call23 = +(numeric__str),let var117 = call23.1,let call22 = +(numeric__str),let var115 = call22.1,let var116 = var115(call22.0, LString "\"", x),let var118 = var117(call23.0, var116, LString "\""),return var118] else []]
},let show__str = [{0: {}, 1: @lambda25}],function @lambda26(@lambda26_env, self, i) {
  return get_index_str(self, i)
},let indexable__str_char = [{0: {}, 1: @lambda26}],function join_str(c, sep) {
  sif (length c == LInt 0) then return LString "" else sif (length c == LInt 1) then return c.0 else sif (length c > 0) then let call25 = +(numeric__str), let var121 = call25.1, let call24 = +(numeric__str), let var119 = call24.1, let var120 = var119(call24.0, c.0, sep), let var122 = var121(call25.0, var120, join_str(c[1], sep)), return var122 else 
},function @lambda27(@lambda27_env, x) {
  return x
},let to_str__str = [{0: {}, 1: @lambda27}],function unlines(lines) {
  let call26 = to_str(to_str__str)
  let var123 = call26.1
  let var124 = var123(call26.0, join_str(lines, LString "\n"))
  return var124
},native function native.plmc.add_int 2,native function native.plmc.sub_int 2,native function native.plmc.mul_int 2,native function native.plmc.div_int 2,native function native.plmc.mod_int 2,native function native.plmc.float_to_int 1,native function native.plmc.int_to_float 1,native function native.plmc.eq_int 2,native function native.plmc.lt_int 2,native function native.plmc.add_float 2,native function native.plmc.sub_float 2,native function native.plmc.mul_float 2,native function native.plmc.div_float 2,native function native.plmc.mod_float 2,native function native.plmc.pow_float 2,native function native.plmc.eq_float 2,native function native.plmc.lt_float 2,function @lambda28(@lambda28_env, c, d) {
  return add_int(c, d)
},function @lambda29(@lambda29_env, x, y) {
  return sub_int(x, y)
},let numeric__int = [{0: {}, 1: @lambda28}, {0: {}, 1: @lambda29}],function @lambda30(@lambda30_env, c, d) {
  return mul_int(c, d)
},function @lambda31(@lambda31_env, c, d) {
  return div_int(c, d)
},function @lambda32(@lambda32_env, c, d) {
  return if d == LInt 0 then [return LInt 1] else [if LBool True then [let call29 = *(product__int),let var129 = call29.1,let call28 = ^(product__int),let var127 = call28.1,let call27 = -(numeric__int),let var125 = call27.1,let var126 = var125(call27.0, d, LInt 1),let var128 = var127(call28.0, c, var126),let var130 = var129(call29.0, c, var128),return var130] else []]
},let product__int = [{0: {}, 1: @lambda30}, {0: {}, 1: @lambda31}, {0: {}, 1: @lambda32}],function @lambda33(@lambda33_env) {
  return LInt 0
},let default__int = [{0: {}, 1: @lambda33}],function @lambda34(@lambda34_env, c, _) {
  return to_string(c)
},let show__int = [{0: {}, 1: @lambda34}],function %(c, d) {
  return mod_int(c, d)
},function @lambda35(@lambda35_env, c) {
  return to_string(c)
},let to_str__int = [{0: {}, 1: @lambda35}],function @lambda36(@lambda36_env, x, y) {
  return eq_int(x, y)
},let equality__int = [{0: {}, 1: @lambda36}],function @lambda37(@lambda37_env, x, y) {
  return lt_int(x, y)
},let ordering__int = [{0: {}, 1: @lambda37}],function @lambda38(@lambda38_env, c, d) {
  return add_float(c, d)
},function @lambda39(@lambda39_env, x, y) {
  return sub_float(x, y)
},let numeric__float = [{0: {}, 1: @lambda38}, {0: {}, 1: @lambda39}],function @lambda40(@lambda40_env, c, d) {
  return mul_float(c, d)
},function @lambda41(@lambda41_env, c, d) {
  return div_float(c, d)
},function @lambda42(@lambda42_env, c, d) {
  return pow_float(c, int_to_float(d))
},let product__float = [{0: {}, 1: @lambda40}, {0: {}, 1: @lambda41}, {0: {}, 1: @lambda42}],function @lambda43(@lambda43_env, x, y) {
  return eq_float(x, y)
},let equality__float = [{0: {}, 1: @lambda43}],function @lambda44(@lambda44_env) {
  return LFloat 0.0
},let default__float = [{0: {}, 1: @lambda44}],function @lambda45(@lambda45_env, x, y) {
  return lt_float(x, y)
},let ordering__float = [{0: {}, 1: @lambda45}],function @lambda46(@lambda46_env, c, _) {
  return to_string(c)
},let show__float = [{0: {}, 1: @lambda46}],function @lambda47(@lambda47_env, c) {
  return to_string(c)
},let to_str__float = [{0: {}, 1: @lambda47}],function expf(x) {
  return pow_float(LFloat 2.718282, x)
},function exp(x) {
  return expf(int_to_float(x))
},function to_int(x) {
  return float_to_int(x)
},function to_float(x) {
  return int_to_float(x)
},function tuple(a0, a1) {
  return [special, LString "tuple", LString "tuple", a0, a1]
},function @lambda48(@lambda48_env, t, prec) {
  let var23 = @lambda48_env.0
  let var24 = @lambda48_env.1
  return if if t.0 == special then [t.2 == LString "tuple"] else [LBool False] then [let call37 = +(numeric__str),let var145 = call37.1,let call36 = +(numeric__str),let var143 = call36.1,let call33 = +(numeric__str),let var137 = call33.1,let call32 = +(numeric__str),let var135 = call32.1,let call31 = show_prec(@lambda48_env.0),let var133 = call31.1,let call30 = +(numeric__int),let var131 = call30.1,let var132 = var131(call30.0, prec, LInt 1),let var134 = var133(call31.0, t.3, var132),let var136 = var135(call32.0, LString "(", var134),let var138 = var137(call33.0, var136, LString ", "),let call35 = show_prec(@lambda48_env.1),let var141 = call35.1,let call34 = +(numeric__int),let var139 = call34.1,let var140 = var139(call34.0, prec, LInt 1),let var142 = var141(call35.0, t.4, var140),let var144 = var143(call36.0, var138, var142),let var146 = var145(call37.0, var144, LString ")"),return var146] else []
},function show__tuple_tvar_tvar(var23, var24) {
  return [{0: {0: var23, 1: var24}, 1: @lambda48}]
},function @lambda49(@lambda49_env, a, b) {
  let var27 = @lambda49_env.0
  let var28 = @lambda49_env.1
  let $switch0 = tuple(a, b)
  return if if $switch0.0 == special then [if $switch0.2 == LString "tuple" then [if $switch0.3.0 == special then [if $switch0.3.2 == LString "tuple" then [if $switch0.4.0 == special then [$switch0.4.2 == LString "tuple"] else [LBool False]] else [LBool False]] else [LBool False]] else [LBool False]] else [LBool False] then [let call40 = and(boolean_algebra__bool),let var151 = call40.1,let call38 = ==(@lambda49_env.0),let var147 = call38.1,let var148 = var147(call38.0, $switch0.3.3, $switch0.4.3),let call39 = ==(@lambda49_env.1),let var149 = call39.1,let var150 = var149(call39.0, $switch0.3.4, $switch0.4.4),let var152 = var151(call40.0, var148, var150),return var152] else []
},function equality__tuple_tvar_tvar(var27, var28) {
  return [{0: {0: var27, 1: var28}, 1: @lambda49}]
},function fst(t) {
  sif (if t.0 == special then [t.2 == LString "tuple"] else [LBool False]) then return t.3 else 
},function snd(t) {
  sif (if t.0 == special then [t.2 == LString "tuple"] else [LBool False]) then return t.4 else 
},function Some(a0) {
  return [special, LString "Option", LString "Some", a0]
},let None = [special, LString "Option", LString "None"],function when(cond, a) {
  sif (cond) then let call41 = a, let var153 = call41.1, let var154 = var153(call41.0), return Some(var154) else return None
},function unless(cond, a) {
  sif (cond) then return None else let call42 = a, let var155 = call42.1, let var156 = var155(call42.0), return Some(var156)
},function Return(a0) {
  return [special, LString "Control", LString "Return", a0]
},let Break = [special, LString "Control", LString "Break"],let Continue = [special, LString "Control", LString "Continue"],function while_(cond, body) {
  let call43 = cond
  let var157 = call43.1
  let var158 = var157(call43.0)
  sif (var158) then let call44 = body, let var159 = call44.1, let var160 = var159(call44.0), let $switch1 = var160, sif (if $switch1.0 == special then [$switch1.2 == LString "Return"] else [LBool False]) then return Some($switch1.3), nil else sif (if $switch1.0 == special then [$switch1.2 == LString "Break"] else [LBool False]) then return None, nil else sif (if $switch1.0 == special then [$switch1.2 == LString "Continue"] else [LBool False]) then return while_(cond, body), nil else  else return None
},function @lambda50(@lambda50_env, o, prec) {
  let var31 = @lambda50_env.0
  return if if o.0 == special then [o.2 == LString "Some"] else [LBool False] then [let call48 = +(numeric__str),let var167 = call48.1,let call47 = +(numeric__str),let var165 = call47.1,let call46 = show_prec(@lambda50_env.0),let var163 = call46.1,let call45 = +(numeric__int),let var161 = call45.1,let var162 = var161(call45.0, prec, LInt 1),let var164 = var163(call46.0, o.3, var162),let var166 = var165(call47.0, LString "Some(", var164),let var168 = var167(call48.0, var166, LString ")"),return var168] else [if if o.0 == special then [o.2 == LString "None"] else [LBool False] then [return LString "None"] else []]
},function show__Option_tvar(var31) {
  return [{0: {0: var31}, 1: @lambda50}]
},function @lambda51(@lambda51_env, a, b) {
  return if if a.0 == special then [a.2 == LString "Some"] else [LBool False] then [return b] else [if if a.0 == special then [a.2 == LString "None"] else [LBool False] then [return None] else []]
},function @lambda52(@lambda52_env, a) {
  return None
},function @lambda53(@lambda53_env, a, b) {
  return if if a.0 == special then [a.2 == LString "Some"] else [LBool False] then [return a] else [if if a.0 == special then [a.2 == LString "None"] else [LBool False] then [return b] else []]
},let boolean_algebra__Option_tvar = [{0: {}, 1: @lambda51}, {0: {}, 1: @lambda52}, {0: {}, 1: @lambda53}],function or_else(a, b) {
  sif (if a.0 == special then [a.2 == LString "Some"] else [LBool False]) then return a.3 else sif (if a.0 == special then [a.2 == LString "None"] else [LBool False]) then return b else 
},function @lambda54(@lambda54_env, a, f) {
  return if if a.0 == special then [a.2 == LString "Some"] else [LBool False] then [let call49 = f,let var169 = call49.1,let var170 = var169(call49.0, a.3),return Some(var170)] else [if if a.0 == special then [a.2 == LString "None"] else [LBool False] then [return None] else []]
},let traversable__Option = [{0: {}, 1: @lambda54}],function @lambda55(@lambda55_env, a, f) {
  return if if a.0 == special then [a.2 == LString "Some"] else [LBool False] then [let call50 = f,let var171 = call50.1,let var172 = var171(call50.0, a.3),return var172] else [if if a.0 == special then [a.2 == LString "None"] else [LBool False] then [return None] else []]
},function @lambda56(@lambda56_env, a) {
  return Some(a)
},let monadic__Option = [{0: {}, 1: @lambda55}, {0: {}, 1: @lambda56}],function simplify_option(x) {
  sif (if x.0 == special then [if x.2 == LString "Some" then [if x.3.0 == special then [x.3.2 == LString "Some"] else [LBool False]] else [LBool False]] else [LBool False]) then return Some(x.3.3), return nil else return None, return nil
},function @lambda57(@lambda57_env, a, b) {
  let var33 = @lambda57_env.0
  let $switch2 = tuple(a, b)
  return if if $switch2.0 == special then [if $switch2.2 == LString "tuple" then [if $switch2.3.0 == special then [if $switch2.3.2 == LString "Some" then [if $switch2.4.0 == special then [$switch2.4.2 == LString "Some"] else [LBool False]] else [LBool False]] else [LBool False]] else [LBool False]] else [LBool False] then [let call51 = ==(@lambda57_env.0),let var173 = call51.1,let var174 = var173(call51.0, $switch2.3.3, $switch2.4.3),return var174] else [if if $switch2.0 == special then [if $switch2.2 == LString "tuple" then [if $switch2.3.0 == special then [if $switch2.3.2 == LString "None" then [if $switch2.4.0 == special then [$switch2.4.2 == LString "None"] else [LBool False]] else [LBool False]] else [LBool False]] else [LBool False]] else [LBool False] then [return LBool True] else [if LBool True then [return LBool False] else []]]
},function equality__Option_tvar(var33) {
  return [{0: {0: var33}, 1: @lambda57}]
},function is_some(x) {
  sif (if x.0 == special then [x.2 == LString "Some"] else [LBool False]) then return LBool True else sif (if x.0 == special then [x.2 == LString "None"] else [LBool False]) then return LBool False else 
},function is_none(x) {
  let call52 = not(boolean_algebra__bool)
  let var175 = call52.1
  let var176 = var175(call52.0, is_some(x))
  return var176
},native function native.plmc.list_concat 2,native function native.plmc.ffi_get_index 2,native function native.plmc.ffi_slice_list 3,function @lambda58(@lambda58_env, self, i) {
  return ffi_get_index(self, i)
},let indexable__list_tvar_tvar = [{0: {}, 1: @lambda58}],function @lambda59(@lambda59_env, self, f) {
  return if length self == LInt 0 then [return []] else [if length self > 0 then [let call53 = f,let var177 = call53.1,let var178 = var177(call53.0, self.0),let call54 = map(traversable__list),let var179 = call54.1,let var180 = var179(call54.0, self[1], f),return list_concat([var178], var180)] else []]
},let traversable__list = [{0: {}, 1: @lambda59}],function @lambda60(@lambda60_env, self, f, init) {
  return if length self == LInt 0 then [return init] else [if length self > 0 then [let call56 = foldl(foldable__list),let var183 = call56.1,let call55 = f,let var181 = call55.1,let var182 = var181(call55.0, init, self.0),let var184 = var183(call56.0, self[1], f, var182),return var184] else []]
},let foldable__list = [{0: {}, 1: @lambda60}],function @lambda61(@lambda61_env, acc, _) {
  let call57 = +(numeric__int)
  let var185 = call57.1
  let var186 = var185(call57.0, acc, LInt 1)
  return var186
},function @lambda62(@lambda62_env, x) {
  let @lambda61 = @lambda62_env.0
  let var35 = @lambda62_env.1
  let call58 = foldl(@lambda62_env.1)
  let var187 = call58.1
  let var188 = var187(call58.0, x, {0: {}, 1: @lambda62_env.0}, LInt 0)
  return var188
},function len(var35) {
  return {0: {0: @lambda61, 1: var35}, 1: @lambda62}
},function @lambda63(@lambda63_env, acc, y) {
  let f = @lambda63_env.0
  let call59 = @lambda63_env.0
  let var189 = call59.1
  let var190 = var189(call59.0, y)
  return if var190 then [return list_concat(acc, [y])] else [return acc]
  return nil
},function filter(x, f) {
  let call60 = foldl(foldable__list)
  let var191 = call60.1
  let var192 = var191(call60.0, x, {0: {0: f}, 1: @lambda63}, [])
  return var192
},function @lambda64(@lambda64_env, acc, z) {
  let var37 = @lambda64_env.0
  let y = @lambda64_env.1
  let call62 = or(boolean_algebra__bool)
  let var195 = call62.1
  let call61 = ==(@lambda64_env.0)
  let var193 = call61.1
  let var194 = var193(call61.0, z, @lambda64_env.1)
  let var196 = var195(call62.0, acc, var194)
  return var196
},function @lambda65(@lambda65_env, x, y) {
  let @lambda64 = @lambda65_env.0
  let var36 = @lambda65_env.1
  let var37 = @lambda65_env.2
  let call63 = foldl(@lambda65_env.1)
  let var197 = call63.1
  let var198 = var197(call63.0, x, {0: {0: @lambda65_env.2, 1: y}, 1: @lambda65_env.0}, LBool False)
  return var198
},function elem(var36, var37) {
  return {0: {0: @lambda64, 1: var36, 2: var37}, 1: @lambda65}
},function @lambda66(@lambda66_env, ls, prec) {
  let var40 = @lambda66_env.0
  return if length ls == LInt 0 then [return LString ""] else [if length ls == LInt 1 then [let call65 = show_prec(@lambda66_env.0),let var201 = call65.1,let call64 = +(numeric__int),let var199 = call64.1,let var200 = var199(call64.0, prec, LInt 1),let var202 = var201(call65.0, ls.0, var200),return var202] else [if length ls > 0 then [let call70 = +(numeric__str),let var211 = call70.1,let call68 = +(numeric__str),let var207 = call68.1,let call67 = show_prec(@lambda66_env.0),let var205 = call67.1,let call66 = +(numeric__int),let var203 = call66.1,let var204 = var203(call66.0, prec, LInt 1),let var206 = var205(call67.0, ls.0, var204),let var208 = var207(call68.0, var206, LString ", "),let call69 = showListHelper(@lambda66_env.0),let var209 = call69.1,let var210 = var209(call69.0, ls[1], prec),let var212 = var211(call70.0, var208, var210),return var212] else []]]
},function showListHelper(var40) {
  return {0: {0: var40}, 1: @lambda66}
},function @lambda67(@lambda67_env, self, prec) {
  let var41 = @lambda67_env.0
  let call73 = +(numeric__str)
  let var217 = call73.1
  let call72 = +(numeric__str)
  let var215 = call72.1
  let call71 = showListHelper(@lambda67_env.0)
  let var213 = call71.1
  let var214 = var213(call71.0, self, prec)
  let var216 = var215(call72.0, LString "[", var214)
  let var218 = var217(call73.0, var216, LString "]")
  return var218
},function show__list_tvar(var41) {
  return [{0: {0: var41}, 1: @lambda67}]
},function @lambda68(@lambda68_env, x, y) {
  return list_concat(x, y)
},function @lambda69(@lambda69_env, z) {
  let var43 = @lambda69_env.0
  let y = @lambda69_env.1
  let call75 = not(boolean_algebra__bool)
  let var221 = call75.1
  let call74 = elem(foldable__list, @lambda69_env.0)
  let var219 = call74.1
  let var220 = var219(call74.0, @lambda69_env.1, z)
  let var222 = var221(call75.0, var220)
  return var222
},function @lambda70(@lambda70_env, x, y) {
  let @lambda69 = @lambda70_env.0
  let var43 = @lambda70_env.1
  return filter(x, {0: {0: @lambda70_env.1, 1: y}, 1: @lambda70_env.0})
},function numeric__list_tvar(var43) {
  return [{0: {}, 1: @lambda68}, {0: {0: @lambda69, 1: var43}, 1: @lambda70}]
},function @lambda71(@lambda71_env, x, y) {
  let var45 = @lambda71_env.0
  let $switch3 = tuple(x, y)
  return if if $switch3.0 == special then [if $switch3.2 == LString "tuple" then [if length $switch3.3 == LInt 0 then [length $switch3.4 == LInt 0] else [LBool False]] else [LBool False]] else [LBool False] then [return LBool True] else [if if $switch3.0 == special then [if $switch3.2 == LString "tuple" then [length $switch3.3 == LInt 0] else [LBool False]] else [LBool False] then [return LBool False] else [if if $switch3.0 == special then [if $switch3.2 == LString "tuple" then [length $switch3.4 == LInt 0] else [LBool False]] else [LBool False] then [return LBool False] else [if if $switch3.0 == special then [if $switch3.2 == LString "tuple" then [if length $switch3.3 > 0 then [length $switch3.4 > 0] else [LBool False]] else [LBool False]] else [LBool False] then [let call78 = and(boolean_algebra__bool),let var227 = call78.1,let call76 = ==(@lambda71_env.0),let var223 = call76.1,let var224 = var223(call76.0, $switch3.3.0, $switch3.4.0),let call77 = ==(equality__list_tvar(@lambda71_env.0)),let var225 = call77.1,let var226 = var225(call77.0, $switch3.3[1], $switch3.4[1]),let var228 = var227(call78.0, var224, var226),return var228] else []]]]
},function equality__list_tvar(var45) {
  return [{0: {0: var45}, 1: @lambda71}]
},function slice(xs, start) {
  let $switch4 = tuple(xs, start)
  sif (if $switch4.0 == special then [if $switch4.2 == LString "tuple" then [length $switch4.3 == LInt 0] else [LBool False]] else [LBool False]) then return [] else sif (if $switch4.0 == special then [if $switch4.2 == LString "tuple" then [if length $switch4.3 > 0 then [$switch4.4 == LInt 0] else [LBool False]] else [LBool False]] else [LBool False]) then return xs else sif (if $switch4.0 == special then [if $switch4.2 == LString "tuple" then [length $switch4.3 > 0] else [LBool False]] else [LBool False]) then let call79 = -(numeric__int), let var229 = call79.1, let var230 = var229(call79.0, start, LInt 1), return slice($switch4.3[1], var230) else 
},function @lambda72(@lambda72_env, acc, tp) {
  let f = @lambda72_env.0
  return if if tp.0 == special then [tp.2 == LString "tuple"] else [LBool False] then [let call80 = @lambda72_env.0,let var231 = call80.1,let var232 = var231(call80.0, tp.3),return if var232 then [return Some(tp.4)] else [return acc],return nil] else []
},function @lambda73(@lambda73_env, x, f) {
  let @lambda72 = @lambda73_env.0
  let var48 = @lambda73_env.1
  let call81 = foldl(@lambda73_env.1)
  let var233 = call81.1
  let var234 = var233(call81.0, x, {0: {0: f}, 1: @lambda73_env.0}, None)
  return var234
},function find(var48) {
  return {0: {0: @lambda72, 1: var48}, 1: @lambda73}
},function take_while(x, f) {
  sif (length x == LInt 0) then return [] else sif (length x > 0) then let call82 = f, let var235 = call82.1, let var236 = var235(call82.0, x.0), return if var236 then [return list_concat(take_while(x[1], f), [x.0])] else [return []], return nil else 
},function drop_while(x, f) {
  sif (length x == LInt 0) then return [] else sif (length x > 0) then let call83 = f, let var237 = call83.1, let var238 = var237(call83.0, x.0), return if var238 then [return drop_while(x[1], f)] else [return x], return nil else 
},function span(x, f) {
  return tuple(take_while(x, f), slice(drop_while(x, f), LInt 1))
},function @lambda74(@lambda74_env, z) {
  let var50 = @lambda74_env.0
  let y = @lambda74_env.1
  let call84 = !=(@lambda74_env.0)
  let var239 = call84.1
  let var240 = var239(call84.0, z, @lambda74_env.1)
  return var240
},function @lambda75(@lambda75_env, x, y) {
  let @lambda74 = @lambda75_env.0
  let var50 = @lambda75_env.1
  return if length x == LInt 0 then [return []] else [if LBool True then [let $switch5 = span(x, {0: {0: @lambda75_env.1, 1: y}, 1: @lambda75_env.0}),return if if $switch5.0 == special then [$switch5.2 == LString "tuple"] else [LBool False] then [let call85 = split_on(@lambda75_env.1),let var241 = call85.1,let var242 = var241(call85.0, $switch5.4, y),return list_concat(var242, [$switch5.3])] else []] else []]
},function split_on(var50) {
  return {0: {0: @lambda74, 1: var50}, 1: @lambda75}
},function init(x) {
  sif (length x == LInt 0) then return [] else sif (length x == LInt 1) then return [] else sif (length x > 0) then return list_concat(init(x[1]), [x.0]) else 
},function join(x, y) {
  sif (length x == LInt 0) then return [] else sif (length x == LInt 1) then return [x.0] else sif (length x > 0) then return list_concat([x.0, y], join(x[1], y)) else 
},function @lambda76(@lambda76_env) {
  return []
},let default__list_tvar = [{0: {}, 1: @lambda76}],function @lambda77(@lambda77_env, z) {
  let var52 = @lambda77_env.0
  let y = @lambda77_env.1
  let call86 = !=(@lambda77_env.0)
  let var243 = call86.1
  let var244 = var243(call86.0, z, @lambda77_env.1)
  return var244
},function @lambda78(@lambda78_env, x) {
  let @lambda77 = @lambda78_env.0
  let var52 = @lambda78_env.1
  return if length x == LInt 0 then [return []] else [if length x > 0 then [let call87 = nub(@lambda78_env.1),let var245 = call87.1,let var246 = var245(call87.0, filter(x[1], {0: {0: @lambda78_env.1, 1: x.0}, 1: @lambda78_env.0})),return list_concat(var246, [x.0])] else []]
},function nub(var52) {
  return {0: {0: @lambda77, 1: var52}, 1: @lambda78}
},native function native.plmc.char_to_string 1,native function native.plmc.eq_char 2,function @lambda79(@lambda79_env, c, prec) {
  return if prec == LInt 0 then [return char_to_string(c)] else [if LBool True then [let call89 = +(numeric__str),let var249 = call89.1,let call88 = +(numeric__str),let var247 = call88.1,let var248 = var247(call88.0, LString "'", char_to_string(c)),let var250 = var249(call89.0, var248, LString "'"),return var250] else []]
},let show__char = [{0: {}, 1: @lambda79}],function @lambda80(@lambda80_env, self) {
  return char_to_string(self)
},let to_str__char = [{0: {}, 1: @lambda80}],function @lambda81(@lambda81_env, c, d) {
  return eq_char(c, d)
},let equality__char = [{0: {}, 1: @lambda81}],function @lambda82(@lambda82_env, char_to_string_arg0) {
  return char_to_string(char_to_string_arg0)
},function @lambda83(@lambda83_env, c) {
  let @lambda82 = @lambda83_env.0
  let call90 = map(traversable__list)
  let var251 = call90.1
  let var252 = var251(call90.0, c, {0: {}, 1: @lambda83_env.0})
  return join_str(var252, LString "")
},let to_str__list_char = [{0: {0: @lambda82}, 1: @lambda83}],function Ok(a0) {
  return [special, LString "Result", LString "Ok", a0]
},function Error(a0) {
  return [special, LString "Result", LString "Error", a0]
},function @lambda84(@lambda84_env, self, prec) {
  let var53 = @lambda84_env.0
  let var54 = @lambda84_env.1
  return if if self.0 == special then [self.2 == LString "Error"] else [LBool False] then [let call94 = +(numeric__str),let var259 = call94.1,let call93 = +(numeric__str),let var257 = call93.1,let call92 = show_prec(@lambda84_env.1),let var255 = call92.1,let call91 = +(numeric__int),let var253 = call91.1,let var254 = var253(call91.0, prec, LInt 1),let var256 = var255(call92.0, self.3, var254),let var258 = var257(call93.0, LString "Error(", var256),let var260 = var259(call94.0, var258, LString ")"),return var260] else [if if self.0 == special then [self.2 == LString "Ok"] else [LBool False] then [let call98 = +(numeric__str),let var267 = call98.1,let call97 = +(numeric__str),let var265 = call97.1,let call96 = show_prec(@lambda84_env.0),let var263 = call96.1,let call95 = +(numeric__int),let var261 = call95.1,let var262 = var261(call95.0, prec, LInt 1),let var264 = var263(call96.0, self.3, var262),let var266 = var265(call97.0, LString "Ok(", var264),let var268 = var267(call98.0, var266, LString ")"),return var268] else []]
},function show__Result_tvar_tvar(var53, var54) {
  return [{0: {0: var53, 1: var54}, 1: @lambda84}]
},function or_error(a, f) {
  sif (if a.0 == special then [a.2 == LString "Some"] else [LBool False]) then return Ok(a.3) else sif (if a.0 == special then [a.2 == LString "None"] else [LBool False]) then let call99 = f, let var269 = call99.1, let var270 = var269(call99.0), return Error(var270) else 
},function compose(a, b) {
  sif (if a.0 == special then [a.2 == LString "Ok"] else [LBool False]) then return Ok(a.3) else sif (if a.0 == special then [a.2 == LString "Error"] else [LBool False]) then return b else 
},function |>(a, b) {
  return compose(a, b)
},function Node(a0, a1, a2) {
  return [special, LString "Map", LString "Node", a0, a1, a2]
},let Empty = [special, LString "Map", LString "Empty"],function @lambda85(@lambda85_env, m, i) {
  let var59 = @lambda85_env.0
  return if if m.0 == special then [if m.2 == LString "Node" then [if m.5.0 == special then [m.5.2 == LString "Empty"] else [LBool False]] else [LBool False]] else [LBool False] then [let call103 = +(numeric__str),let var277 = call103.1,let call100 = +(numeric__str),let var271 = call100.1,let var272 = var271(call100.0, m.3, LString " -> "),let call102 = show_prec(@lambda85_env.0),let var275 = call102.1,let call101 = +(numeric__int),let var273 = call101.1,let var274 = var273(call101.0, i, LInt 1),let var276 = var275(call102.0, m.4, var274),let var278 = var277(call103.0, var272, var276),return var278] else [if if m.0 == special then [m.2 == LString "Node"] else [LBool False] then [let call110 = +(numeric__str),let var291 = call110.1,let call108 = +(numeric__str),let var287 = call108.1,let call107 = +(numeric__str),let var285 = call107.1,let call104 = +(numeric__str),let var279 = call104.1,let var280 = var279(call104.0, m.3, LString " -> "),let call106 = show_prec(@lambda85_env.0),let var283 = call106.1,let call105 = +(numeric__int),let var281 = call105.1,let var282 = var281(call105.0, i, LInt 1),let var284 = var283(call106.0, m.4, var282),let var286 = var285(call107.0, var280, var284),let var288 = var287(call108.0, var286, LString ", "),let call109 = showHelper(@lambda85_env.0),let var289 = call109.1,let var290 = var289(call109.0, m.5, i),let var292 = var291(call110.0, var288, var290),return var292] else [if if m.0 == special then [m.2 == LString "Empty"] else [LBool False] then [return LString ""] else []]]
},function showHelper(var59) {
  return {0: {0: var59}, 1: @lambda85}
},function @lambda86(@lambda86_env, m, i) {
  let var60 = @lambda86_env.0
  let call113 = +(numeric__str)
  let var297 = call113.1
  let call112 = +(numeric__str)
  let var295 = call112.1
  let call111 = showHelper(@lambda86_env.0)
  let var293 = call111.1
  let var294 = var293(call111.0, m, i)
  let var296 = var295(call112.0, LString "{ ", var294)
  let var298 = var297(call113.0, var296, LString " }")
  return var298
},function show__Map_tvar(var60) {
  return [{0: {0: var60}, 1: @lambda86}]
},function empty() {
  return Empty
},function insert(k, v, m) {
  sif (if m.0 == special then [m.2 == LString "Node"] else [LBool False]) then let call114 = ==(equality__str), let var299 = call114.1, let var300 = var299(call114.0, m.3, k), return if var300 then [return Node(m.3, v, m.5)] else [return Node(m.3, m.4, insert(k, v, m.5))], return nil else sif (if m.0 == special then [m.2 == LString "Node"] else [LBool False]) then return Node(m.3, m.4, insert(k, v, m.5)) else sif (if m.0 == special then [m.2 == LString "Empty"] else [LBool False]) then return Node(k, v, Empty) else 
},function @lambda87(@lambda87_env, acc, el) {
  return if if el.0 == special then [el.2 == LString "tuple"] else [LBool False] then [return insert(el.3, el.4, acc)] else []
},function from_list(l) {
  let call115 = foldl(foldable__list)
  let var301 = call115.1
  let var302 = var301(call115.0, l, {0: {}, 1: @lambda87}, Empty)
  return var302
},function @lambda88(@lambda88_env, m) {
  let var63 = @lambda88_env.0
  return if if m.0 == special then [m.2 == LString "Node"] else [LBool False] then [let call117 = +(numeric__list_tvar(equality__tuple_tvar_tvar(@lambda88_env.0))),let var305 = call117.1,let call116 = to_list(@lambda88_env.0),let var303 = call116.1,let var304 = var303(call116.0, m.5),let var306 = var305(call117.0, [tuple(m.3, m.4)], var304),return var306] else [if if m.0 == special then [m.2 == LString "Empty"] else [LBool False] then [return []] else []]
},function to_list(var63) {
  return {0: {0: var63}, 1: @lambda88}
},function get(m, k) {
  sif (if m.0 == special then [m.2 == LString "Node"] else [LBool False]) then let call118 = ==(equality__str), let var307 = call118.1, let var308 = var307(call118.0, m.3, k), return if var308 then [return Some(m.4)] else [return get(m.5, k)], return nil else sif (if m.0 == special then [m.2 == LString "Node"] else [LBool False]) then return get(m.5, k) else sif (if m.0 == special then [m.2 == LString "Empty"] else [LBool False]) then return None else 
},function @lambda89(@lambda89_env, m, f, acc) {
  return if if m.0 == special then [m.2 == LString "Node"] else [LBool False] then [let call120 = foldl(foldable__Map),let var311 = call120.1,let call119 = f,let var309 = call119.1,let var310 = var309(call119.0, acc, m.4),let var312 = var311(call120.0, m.5, f, var310),return var312] else [if if m.0 == special then [m.2 == LString "Empty"] else [LBool False] then [return acc] else []]
},let foldable__Map = [{0: {}, 1: @lambda89}],function @lambda90(@lambda90_env, c, prec) {
  let var64 = @lambda90_env.0
  let call123 = +(numeric__str)
  let var317 = call123.1
  let call122 = show_prec(@lambda90_env.0)
  let var315 = call122.1
  let call121 = +(numeric__int)
  let var313 = call121.1
  let var314 = var313(call121.0, prec, LInt 1)
  let var316 = var315(call122.0, unmut c, var314)
  let var318 = var317(call123.0, LString "mut ", var316)
  return var318
},function show__mut_tvar(var64) {
  return [{0: {0: var64}, 1: @lambda90}]
},native function native.plmc.read_file 1,native function native.plmc.write_file 2,native function native.plmc.append_file 2,native function native.plmc.does_file_exist 1,native function native.plmc.ffi_print 1,native function native.plmc.ffi_println 1,function @lambda91(@lambda91_env, x) {
  let var66 = @lambda91_env.0
  let call124 = show(@lambda91_env.0)
  let var319 = call124.1
  let var320 = var319(call124.0, x)
  ffi_print(var320)
  ffi_print(LString " ")
  return unit
},function @lambda92(@lambda92_env, xs) {
  let @lambda91 = @lambda92_env.0
  let var66 = @lambda92_env.1
  let call125 = map(traversable__list)
  let var321 = call125.1
  let var322 = var321(call125.0, xs, {0: {0: @lambda92_env.1}, 1: @lambda92_env.0})
  var322
  return unit
},function vprint(var66) {
  return {0: {0: @lambda91, 1: var66}, 1: @lambda92}
},function @lambda93(@lambda93_env, x) {
  let var67 = @lambda93_env.0
  let call126 = show(@lambda93_env.0)
  let var323 = call126.1
  let var324 = var323(call126.0, x)
  ffi_print(var324)
  ffi_print(LString " ")
  return unit
},function @lambda94(@lambda94_env, xs) {
  let @lambda93 = @lambda94_env.0
  let var67 = @lambda94_env.1
  let call127 = map(traversable__list)
  let var325 = call127.1
  let var326 = var325(call127.0, xs, {0: {0: @lambda94_env.1}, 1: @lambda94_env.0})
  var326
  ffi_print(LString "\n")
  return unit
},function vprintln(var67) {
  return {0: {0: @lambda93, 1: var67}, 1: @lambda94}
},function @lambda95(@lambda95_env, x) {
  let var68 = @lambda95_env.0
  let call128 = show(@lambda95_env.0)
  let var327 = call128.1
  let var328 = var327(call128.0, x)
  ffi_print(var328)
  return unit
},function print(var68) {
  return {0: {0: var68}, 1: @lambda95}
},function @lambda96(@lambda96_env, x) {
  let var69 = @lambda96_env.0
  let call129 = show(@lambda96_env.0)
  let var329 = call129.1
  let var330 = var329(call129.0, x)
  ffi_println(var330)
  return unit
},function println(var69) {
  return {0: {0: var69}, 1: @lambda96}
},native function native.plmc.get_args 0,native function native.plmc.execute_command 1,native function native.plmc.input 1,function @lambda97(@lambda97_env, name) {
  let var70 = @lambda97_env.0
  let call133 = println(show__str)
  let var337 = call133.1
  let call132 = +(numeric__str)
  let var335 = call132.1
  let call131 = +(numeric__str)
  let var333 = call131.1
  let call130 = show(@lambda97_env.0)
  let var331 = call130.1
  let var332 = var331(call130.0, name)
  let var334 = var333(call131.0, var332, LString "!")
  let var336 = var335(call132.0, LString "Hello, ", var334)
  let var338 = var337(call133.0, var336)
  var338
  return unit
},function welcome(var70) {
  return {0: {0: var70}, 1: @lambda97}
},let call134 = println(show__str),let var339 = call134.1,let var340 = var339(call134.0, LString "test"),var340,let call135 = welcome(show__str),let var341 = call135.1,let var342 = var341(call135.0, LString "world"),var342,let call136 = welcome(show__int),let var343 = call136.1,let var344 = var343(call136.0, LInt 42),var344,let call137 = welcome(show__str),let var345 = call137.1,let var346 = var345(call137.0, LString "user"),var346]