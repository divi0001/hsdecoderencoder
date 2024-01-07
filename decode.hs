
import Data.Char


decodePlus :: String -> Int -> [[String]]
decodePlus s 0 = [[fromAscii (map (+1) (toAscii s))]]
decodePlus s n = [[fromAscii (map (+n) (toAscii s))]] ++ decodePlus s (n-1)

decodeMinus :: String -> Int -> [[String]]
decodeMinus s 26 = [[fromAscii (map (\x -> x-1) (toAscii s))]]
decodeMinus s n = [[fromAscii (map (\x -> x-n) (toAscii s))]] ++ decodeMinus s (n+1)


toAscii :: String -> [Int]
toAscii (s:ss) = [ord s] ++ map ord ss


fromAscii :: [Int] -> String
fromAscii [] = ""
fromAscii (x:xs) = chr x : fromAscii xs



pretty :: [[String]] -> String
pretty [] = ""
pretty (s:ss) = show s ++ "\n" ++ pretty ss




--vvrjm al rbvfkbt ewjx tbyctthd bztr np gfsn opgli gkkg vnk kgfi"
-- decode minus: (jeweils 26)

-- ["vvrjm al rbvfkbt ewjx tbyctthd bztr np gfsn opgli gkkg vnk kgfi\""]
-- ["uuqil\US`k\USqauejas\USdviw\USsaxbssgc\USaysq\USmo\USferm\USnofkh\USfjjf\USumj\USjfeh!"]
-- ["ttphk\RS_j\RSp`tdi`r\RScuhv\RSr`warrfb\RS`xrp\RSln\RSedql\RSmnejg\RSeiie\RStli\RSiedg "]
-- ["ssogj\GS^i\GSo_sch_q\GSbtgu\GSq_v`qqea\GS_wqo\GSkm\GSdcpk\GSlmdif\GSdhhd\GSskh\GShdcf\US"]
-- ["rrnfi\FS]h\FSn^rbg^p\FSasft\FSp^u_ppd`\FS^vpn\FSjl\FScboj\FSklche\FScggc\FSrjg\FSgcbe\RS"]
-- ["qqmeh\ESC\\g\ESCm]qaf]o\ESC`res\ESCo]t^ooc_\ESC]uom\ESCik\ESCbani\ESCjkbgd\ESCbffb\ESCqif\ESCfbad\GS"]
-- ["ppldg\SUB[f\SUBl\\p`e\\n\SUB_qdr\SUBn\\s]nnb^\SUB\\tnl\SUBhj\SUBa`mh\SUBijafc\SUBaeea\SUBphe\SUBea`c\FS"]
-- ["ookcf\EMZe\EMk[o_d[m\EM^pcq\EMm[r\\mma]\EM[smk\EMgi\EM`_lg\EMhi`eb\EM`dd`\EMogd\EMd`_b\ESC"]
-- ["nnjbe\CANYd\CANjZn^cZl\CAN]obp\CANlZq[ll`\\\CANZrlj\CANfh\CAN_^kf\CANgh_da\CAN_cc_\CANnfc\CANc_^a\SUB"]
-- ["mmiad\ETBXc\ETBiYm]bYk\ETB\\nao\ETBkYpZkk_[\ETBYqki\ETBeg\ETB^]je\ETBfg^c`\ETB^bb^\ETBmeb\ETBb^]`\EM"]
-- ["llh`c\SYNWb\SYNhXl\\aXj\SYN[m`n\SYNjXoYjj^Z\SYNXpjh\SYNdf\SYN]\\id\SYNef]b_\SYN]aa]\SYNlda\SYNa]\\_\CAN"]
-- ["kkg_b\NAKVa\NAKgWk[`Wi\NAKZl_m\NAKiWnXii]Y\NAKWoig\NAKce\NAK\\[hc\NAKde\\a^\NAK\\``\\\NAKkc`\NAK`\\[^\ETB"]
-- ["jjf^a\DC4U`\DC4fVjZ_Vh\DC4Yk^l\DC4hVmWhh\\X\DC4Vnhf\DC4bd\DC4[Zgb\DC4cd[`]\DC4[__[\DC4jb_\DC4_[Z]\SYN"]
-- ["iie]`\DC3T_\DC3eUiY^Ug\DC3Xj]k\DC3gUlVgg[W\DC3Umge\DC3ac\DC3ZYfa\DC3bcZ_\\\DC3Z^^Z\DC3ia^\DC3^ZY\\\NAK"]
-- ["hhd\\_\DC2S^\DC2dThX]Tf\DC2Wi\\j\DC2fTkUffZV\DC2Tlfd\DC2`b\DC2YXe`\DC2abY^[\DC2Y]]Y\DC2h`]\DC2]YX[\DC4"]
-- ["ggc[^\DC1R]\DC1cSgW\\Se\DC1Vh[i\DC1eSjTeeYU\DC1Skec\DC1_a\DC1XWd_\DC1`aX]Z\DC1X\\\\X\DC1g_\\\DC1\\XWZ\DC3"]
-- ["ffbZ]\DLEQ\\\DLEbRfV[Rd\DLEUgZh\DLEdRiSddXT\DLERjdb\DLE^`\DLEWVc^\DLE_`W\\Y\DLEW[[W\DLEf^[\DLE[WVY\DC2"]
-- ["eeaY\\\SIP[\SIaQeUZQc\SITfYg\SIcQhRccWS\SIQica\SI]_\SIVUb]\SI^_V[X\SIVZZV\SIe]Z\SIZVUX\DC1"]
-- ["dd`X[\SOOZ\SO`PdTYPb\SOSeXf\SObPgQbbVR\SOPhb`\SO\\^\SOUTa\\\SO]^UZW\SOUYYU\SOd\\Y\SOYUTW\DLE"]
-- ["cc_WZ\rNY\r_OcSXOa\rRdWe\raOfPaaUQ\rOga_\r[]\rTS`[\r\\]TYV\rTXXT\rc[X\rXTSV\SI"]
-- ["bb^VY\fMX\f^NbRWN`\fQcVd\f`NeO``TP\fNf`^\fZ\\\fSR_Z\f[\\SXU\fSWWS\fbZW\fWSRU\SO"]
-- ["aa]UX\vLW\v]MaQVM_\vPbUc\v_MdN__SO\vMe_]\vY[\vRQ^Y\vZ[RWT\vRVVR\vaYV\vVRQT\r"]
-- ["``\\TW\nKV\n\\L`PUL^\nOaTb\n^LcM^^RN\nLd^\\\nXZ\nQP]X\nYZQVS\nQUUQ\n`XU\nUQPS\f"]
-- ["__[SV\tJU\t[K_OTK]\tN`Sa\t]KbL]]QM\tKc][\tWY\tPO\\W\tXYPUR\tPTTP\t_WT\tTPOR\v"]
-- ["^^ZRU\bIT\bZJ^NSJ\\\bM_R`\b\\JaK\\\\PL\bJb\\Z\bVX\bON[V\bWXOTQ\bOSSO\b^VS\bSONQ\n"]
-- ["]]YQT\aHS\aYI]MRI[\aL^Q_\a[I`J[[OK\aIa[Y\aUW\aNMZU\aVWNSP\aNRRN\a]UR\aRNMP\t"]
-- ["uuqil\US`k\USqauejas\USdviw\USsaxbssgc\USaysq\USmo\USferm\USnofkh\USfjjf\USumj\USjfeh!"]



--decode plus:

-- ["\144\144\140\132\135:{\134:\140|\144\128\133|\142:\DEL\145\132\146:\142|\147}\142\142\130~:|\148\142\140:\136\138:\129\128\141\136:\137\138\129\134\131:\129\133\133\129:\144\136\133:\133\129\128\131<"]
-- ["\143\143\139\131\134\&9z\133\&9\139{\143\DEL\132{\141\&9~\144\131\145\&9\141{\146|\141\141\129}9{\147\141\139\&9\135\137\&9\128\DEL\140\135\&9\136\137\128\133\130\&9\128\132\132\128\&9\143\135\132\&9\132\128\DEL\130;"]
-- ["\142\142\138\130\133\&8y\132\&8\138z\142~\131z\140\&8}\143\130\144\&8\140z\145{\140\140\128|8z\146\140\138\&8\134\136\&8\DEL~\139\134\&8\135\136\DEL\132\129\&8\DEL\131\131\DEL8\142\134\131\&8\131\DEL~\129:"]
-- ["\141\141\137\129\132\&7x\131\&7\137y\141}\130y\139\&7|\142\129\143\&7\139y\144z\139\139\DEL{7y\145\139\137\&7\133\135\&7~}\138\133\&7\134\135~\131\128\&7~\130\130~7\141\133\130\&7\130~}\128\&9"]
-- ["\140\140\136\128\131\&6w\130\&6\136x\140|\129x\138\&6{\141\128\142\&6\138x\143y\138\138~z6x\144\138\136\&6\132\134\&6}|\137\132\&6\133\134}\130\DEL6}\129\129}6\140\132\129\&6\129}|\DEL8"]
-- ["\139\139\135\DEL\130\&5v\129\&5\135w\139{\128w\137\&5z\140\DEL\141\&5\137w\142x\137\137}y5w\143\137\135\&5\131\133\&5|{\136\131\&5\132\133|\129~5|\128\128|5\139\131\128\&5\128|{~7"]
-- ["\138\138\134~\129\&4u\128\&4\134v\138z\DELv\136\&4y\139~\140\&4\136v\141w\136\136|x4v\142\136\134\&4\130\132\&4{z\135\130\&4\131\132{\128}4{\DEL\DEL{4\138\130\DEL4\DEL{z}6"]
-- ["\137\137\133}\128\&3t\DEL3\133u\137y~u\135\&3x\138}\139\&3\135u\140v\135\135{w3u\141\135\133\&3\129\131\&3zy\134\129\&3\130\131z\DEL|3z~~z3\137\129~3~zy|5"]
-- ["\136\136\132|\DEL2s~2\132t\136x}t\134\&2w\137|\138\&2\134t\139u\134\134zv2t\140\134\132\&2\128\130\&2yx\133\128\&2\129\130y~{2y}}y2\136\128}2}yx{4"]
-- ["\135\135\131{~1r}1\131s\135w|s\133\&1v\136{\137\&1\133s\138t\133\133yu1s\139\133\131\&1\DEL\129\&1xw\132\DEL1\128\129x}z1x||x1\135\DEL|1|xwz3"]
-- ["\134\134\130z}0q|0\130r\134v{r\132\&0u\135z\136\&0\132r\137s\132\132xt0r\138\132\130\&0~\128\&0wv\131~0\DEL\128w|y0w{{w0\134~{0{wvy2"]
-- ["\133\133\129y|/p{/\129q\133uzq\131/t\134y\135/\131q\136r\131\131ws/q\137\131\129/}\DEL/vu\130}/~\DELv{x/vzzv/\133}z/zvux1"]
-- ["\132\132\128x{.oz.\128p\132typ\130.s\133x\134.\130p\135q\130\130vr.p\136\130\128.|~.ut\129|.}~uzw.uyyu.\132|y.yutw0"]
-- ["\131\131\DELwz-ny-\DELo\131sxo\129-r\132w\133-\129o\134p\129\129uq-o\135\129\DEL-{}-ts\128{-|}tyv-txxt-\131{x-xtsv/"]
-- ["\130\130~vy,mx,~n\130rwn\128,q\131v\132,\128n\133o\128\128tp,n\134\128~,z|,sr\DELz,{|sxu,swws,\130zw,wsru."]
-- ["\129\129}ux+lw+}m\129qvm\DEL+p\130u\131+\DELm\132n\DEL\DELso+m\133\DEL}+y{+rq~y+z{rwt+rvvr+\129yv+vrqt-"]
-- ["\128\128|tw*kv*|l\128pul~*o\129t\130*~l\131m~~rn*l\132~|*xz*qp}x*yzqvs*quuq*\128xu*uqps,"]
-- ["\DEL\DEL{sv)ju){k\DELotk})n\128s\129)}k\130l}}qm)k\131}{)wy)po|w)xypur)pttp)\DELwt)tpor+"]
-- ["~~zru(it(zj~nsj|(m\DELr\128(|j\129k||pl(j\130|z(vx(on{v(wxotq(osso(~vs(sonq*"]
-- ["}}yqt'hs'yi}mri{'l~q\DEL'{i\128j{{ok'i\129{y'uw'nmzu'vwnsp'nrrn'}ur'rnmp)"]
-- ["||xps&gr&xh|lqhz&k}p~&zh\DELizznj&h\128zx&tv&mlyt&uvmro&mqqm&|tq&qmlo("]
-- ["{{wor%fq%wg{kpgy%j|o}%yg~hyymi%g\DELyw%su%lkxs%tulqn%lppl%{sp%plkn'"]
-- ["zzvnq$ep$vfzjofx$i{n|$xf}gxxlh$f~xv$rt$kjwr$stkpm$kook$zro$okjm&"]
-- ["yyump#do#ueyinew#hzm{#we|fwwkg#e}wu#qs#jivq#rsjol#jnnj#yqn#njil%"]
-- ["xxtlo\"cn\"tdxhmdv\"gylz\"vd{evvjf\"d|vt\"pr\"ihup\"qrink\"immi\"xpm\"mihk$"]
-- ["wwskn!bm!scwglcu!fxky!uczduuie!c{us!oq!hgto!pqhmj!hllh!wol!lhgj#"]
-- ["wwskn!bm!scwglcu!fxky!uczduuie!c{us!oq!hgto!pqhmj!hllh!wol!lhgj#"]


p:: Int -> String
p 100 = "<item>100</item>"
p x = "<item>" ++ show (x) ++  "</item>\n" ++ p (x+1)