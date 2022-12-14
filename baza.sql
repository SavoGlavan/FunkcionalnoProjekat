PGDMP         2        	        z        	   moviesNew    14.3    14.3     ?           0    0    ENCODING    ENCODING     #   SET client_encoding = 'SQL_ASCII';
                      false            ?           0    0 
   STDSTRINGS 
   STDSTRINGS     (   SET standard_conforming_strings = 'on';
                      false            ?           0    0 
   SEARCHPATH 
   SEARCHPATH     8   SELECT pg_catalog.set_config('search_path', '', false);
                      false                        1262    24588 	   moviesNew    DATABASE     o   CREATE DATABASE "moviesNew" WITH TEMPLATE = template0 ENCODING = 'UTF8' LOCALE = 'English_United States.1252';
    DROP DATABASE "moviesNew";
                postgres    false            ?            1259    24590    genre    TABLE     [   CREATE TABLE public.genre (
    id bigint NOT NULL,
    name character varying NOT NULL
);
    DROP TABLE public.genre;
       public         heap    postgres    false            ?            1259    24589    genre_id_seq    SEQUENCE     u   CREATE SEQUENCE public.genre_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
 #   DROP SEQUENCE public.genre_id_seq;
       public          postgres    false    210                       0    0    genre_id_seq    SEQUENCE OWNED BY     =   ALTER SEQUENCE public.genre_id_seq OWNED BY public.genre.id;
          public          postgres    false    209            ?            1259    24599    movies    TABLE     6  CREATE TABLE public.movies (
    id bigint NOT NULL,
    poster character varying NOT NULL,
    title character varying NOT NULL,
    year bigint NOT NULL,
    rating_imdb double precision NOT NULL,
    genre_id bigint NOT NULL,
    director character varying NOT NULL,
    stars character varying NOT NULL
);
    DROP TABLE public.movies;
       public         heap    postgres    false            ?            1259    24598    movies_id_seq    SEQUENCE     v   CREATE SEQUENCE public.movies_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
 $   DROP SEQUENCE public.movies_id_seq;
       public          postgres    false    212                       0    0    movies_id_seq    SEQUENCE OWNED BY     ?   ALTER SEQUENCE public.movies_id_seq OWNED BY public.movies.id;
          public          postgres    false    211            a           2604    24593    genre id    DEFAULT     d   ALTER TABLE ONLY public.genre ALTER COLUMN id SET DEFAULT nextval('public.genre_id_seq'::regclass);
 7   ALTER TABLE public.genre ALTER COLUMN id DROP DEFAULT;
       public          postgres    false    210    209    210            b           2604    24602 	   movies id    DEFAULT     f   ALTER TABLE ONLY public.movies ALTER COLUMN id SET DEFAULT nextval('public.movies_id_seq'::regclass);
 8   ALTER TABLE public.movies ALTER COLUMN id DROP DEFAULT;
       public          postgres    false    212    211    212            ?          0    24590    genre 
   TABLE DATA           )   COPY public.genre (id, name) FROM stdin;
    public          postgres    false    210   ?       ?          0    24599    movies 
   TABLE DATA           a   COPY public.movies (id, poster, title, year, rating_imdb, genre_id, director, stars) FROM stdin;
    public          postgres    false    212   P                  0    0    genre_id_seq    SEQUENCE SET     ;   SELECT pg_catalog.setval('public.genre_id_seq', 1, false);
          public          postgres    false    209                       0    0    movies_id_seq    SEQUENCE SET     =   SELECT pg_catalog.setval('public.movies_id_seq', 139, true);
          public          postgres    false    211            d           2606    24597    genre genre_pkey 
   CONSTRAINT     N   ALTER TABLE ONLY public.genre
    ADD CONSTRAINT genre_pkey PRIMARY KEY (id);
 :   ALTER TABLE ONLY public.genre DROP CONSTRAINT genre_pkey;
       public            postgres    false    210            h           2606    24606    movies movies_pkey 
   CONSTRAINT     P   ALTER TABLE ONLY public.movies
    ADD CONSTRAINT movies_pkey PRIMARY KEY (id);
 <   ALTER TABLE ONLY public.movies DROP CONSTRAINT movies_pkey;
       public            postgres    false    212            j           2606    24610    movies unique_text 
   CONSTRAINT     N   ALTER TABLE ONLY public.movies
    ADD CONSTRAINT unique_text UNIQUE (title);
 <   ALTER TABLE ONLY public.movies DROP CONSTRAINT unique_text;
       public            postgres    false    212            f           2606    24608    genre unique_title 
   CONSTRAINT     M   ALTER TABLE ONLY public.genre
    ADD CONSTRAINT unique_title UNIQUE (name);
 <   ALTER TABLE ONLY public.genre DROP CONSTRAINT unique_title;
       public            postgres    false    210            k           2606    24611    movies movies_genre_id_fkey    FK CONSTRAINT     ?   ALTER TABLE ONLY public.movies
    ADD CONSTRAINT movies_genre_id_fkey FOREIGN KEY (genre_id) REFERENCES public.genre(id) ON UPDATE RESTRICT ON DELETE RESTRICT;
 E   ALTER TABLE ONLY public.movies DROP CONSTRAINT movies_genre_id_fkey;
       public          postgres    false    210    3172    212            ?   w   x???
?0???????^?Jq??7??@??$
?{=???ESd??4?8????Wb??p??????T??EsIV??m?E?<?????????"?V?},>W:?.?kp?L?Y??%?X?%-      ?      x??}?v?Z???B??8}A??1?#???k?:F??_.???Տѣ?oУ????Y?X?8@?i?B?\?n]?*BqN??;"?5'??????#<???o<?e?/??7?K????`ϝ6??u???K3Ӎg_?m4?r?????i??9z?????~?????E??<??h??Y0o???>7?9?????o??????????????{o?ZSU??2)&?Z??sY???jO??B???j?"??wqd?????\??_^?O27Rnx????v*[Kʱ???F7ӵI?rV???3zӂ??t?,uO7??d?????5????G?4?0?%?6^_׮?/k????2?'^??[?AP|?>S?re*ĻHϔ?-?r???K?D?~Uݡa??mz??2??Ƴ?v,?l??j<??l???j???_1o??Kv??;w?d?m???L??C5???????I?&????fٙҳW<?=N?8:?)??2??k?ٮn??ֺ4????:????&?d,????U??{?i<񕇈̨???d?v?$n??o????????\@q????;?H?K??)]?đ?[?i?Y?l28??'?e2St??s???,???y?UK???7??|?8?(??N?m???+??H?T?E?? N?l?d?v??hᦸx??m???k? ??p??L??/D??&?^??<?͋?&kw?Ig??s??MK???]??l­M+?&=??~?!???X?Ƭ?{??J??X??27??s??[8?d?O????QV3ܐ,{q#\A'K8?."?8?7????#??z󲺽*C?0?z8C ??t?f???|?s33K+kf˒!?n???5?gU??ʷ??-.gdß?\=??H?[?ag?????K??2fH"7?%?????T???e^?e???????~}̃7?Ň/???OrQ?0?M?!?c/??Sf!W'O?7?x??@??ޅ??S?????cDf???S?,?1?=????Sg+Ҏ?-?=?<-7???-UFAdpK|Q{ʐ+"???n?9???.DJ?uʃ7?ID%????-?@u?Q??z?5+????P?ՍA??q<???6Pm42U\?#GH?????-q?DƩ?-}??8?6??dn???.E]mV???+,???u>+?<??5???ƶ???۳?Sw?w?xb?C\???ů
??2????Bo߉ @?wܷ?Rj????(2<????????2?c?L ,???7=N???M?@Fj?'???֦]YS???'??r?Dhr??rͰW t5u????x??u-ump??"?$"?>÷ݕ%???"???wӚ??="r*??<??|??9@???)|ݪl!?	߮ŌA????
x?=?z??*??/>?t÷?̖A?..?A׵'\C7V???m?F???i??>?`> <`?g??6⵮V??V??-:?!y????ڄg;???t/?w??|?,t~?????X1??HR:???????gw??)?f"JETW?'TS?}???L???ԣ?k??vж?IƼ????f?A???~ܒ?_??yJg?!?Tk["򴈓T?????8?e?a,?G?.ܺZ=??tݴ???@V:.(?&????????!?3$?^P?????'2?S{on?8??7??^??&?O`1>??\??p?ؒ?A??g? ?;?#}?Cp`c? ?[?8????I]??d-?0??q3?5.9Zw??=PU8l?0= g?9??_??p???2??}?R??8?t?o???????9??#D??? ??
??F? ?.?)x?X?????g??F$I??QoUOĺ???s??9???\E,]??F????t??6xQkܧ?a??̀W? ???#3??Ur?E??X?<U? fD??*a.?G??G?w߸???? >ۈ????ު???9??緈?A},2?0;?y<?$?[??Pq?A㿚	n?[?@????n?E?\8?̼?kij?Y???%?.N?}\{????îh???Oy??/r????"??!?/????_P?? 4?AƴǤ??rs???U?4n<??a??????;?y??x?U???å??Sn;\y?W?/?U?p$?x??28?Hĳ???I??l???4z?øq?WÃg5`Th??0??X?? B?I3
?	y?--;*?p?W?@?@???8?1?U???r(??]px????Փ20?΢c??????'????K??i??a ??^Hʕc??ks?i?G???{A4O$??L t#Ϧ????3s?2?#?(?X?sD?ilW???$???9	A???? ???)T?Ty?qKqS;֜??|B?z????u????d1?:H?J+>B???z?/jmD???~???.ա???? ??KN.ģqD?	
?E?iy???9>?X???Ƴ7?5??32??߅'oB=?V???,p;:???yv?<&?;?ขK?{%?=? ?h:???CJD???[?0ND?uB??@?('????>~6?ߪέ
?9??ȖbW(? ???????oS?-ÞG
J??"#?Uo????!S'm=d??NH????:2?A??j?MZ?1??Xu?0?
?)W??o???{)ͽ?BŘ????ܯ??;? ??7	OA??|?XR?[???[?!?Cu??),o!?????T? ?????aF???C?*~?N!.eI????F?׀?ji5?AP???/DQ?<?0; ??E??@????S???9 ??O(??+Ճ77??f?z???;????m??x???hD/#??T+d}??????????*Ra??F<|?4gJa??FD?3a(??&#?R?;ʜ8* ??|Y,?զ_?=W??"??]??礩{???Y?Q*?ԁBz033??uu??P?}L??L2?'<?8?oo??g???&?A?d ???A	??Y?\ѹ?m?;??s?>s`??o?V????O(H?{mK??C p?n??8????M?嘞Y????~?0??pW??ߏT???t???t??0??[{?*76=?I???"????C?}?#:?0oI[?g鞺>HJ97??'T,?E[?,ǚ??~?$? /?H???a:???S?Ҩ???#hw???qeG?H^? D??N????C?E^???CW??H-8??p?J ??4N<??f???c???	??g???<??0?0`%?rM?W?[?6????&??X?ƾHvԥC?ֈm{??R|????<:12?z??c???F??????0???TK?>??ʤK?o?[Y@???;P8?????????}GvCX??$?Kn4?gD????9?[?C(Bn?rʰ1v?}
?1׺????????/?\4??P?8?y	6??)???Ɓ??RZ?uC??}???@:	?ͩC?????w?&k)pL?!O?'????^??݃5|??+M???T?u??@??4???떂pɪ?????	?Pl???Z/???64????|?(ϫI5??{/?[??ս݈?????O?S=ۏ?S??k??I?GME?(??{e?R?e?k??6?^??Uʓ~?3?{???t??1??<??H??%??	???=?? C????(?????N????}?G??ڬ?h?<=@rG ?Qg-p?5??????0???b??????]??v 
?l?զy?"`(?I?T??(o?\??"??t?:???%?[????0????h>hZ?(n?f?_???@??:7???>??????vԐ??A?????Et?}y?S?wI??w?????-E?;???i<#?3?e?v'?C ?a??2K??& ?????|@9t
d??9???7???ِ??&3??	?3????N6ry????Q*?<]????	U-????H)b???P,+`?@5??k$????e???㣹k?|OQ??L?
??-6lWu?q??>??T[?T?,&??>+@3j?d???y?S?I?[?z
=t???a?g?ڸ?UzJ W??a ?g?)]J?)c ?ȏ__7??jw????T?KDX(?I??9A|ҟENCvܟ?%?l?	?2'    ?D:tt?8?t?|^??()???0????(m;?E?s'??M?c?I6"??Z?H,??h?F6WvE????9E?rv?+L??AS???z>???z	<?7A??} nt?X?QǄ~z??ʭ??S!?8????	OW?????bP'??I??????	!?^TʺG??%?????j*J7????Nud??S?&??k:??$˻d]6????s?e8.N???????@?*?j%???<?" W??ǿv?we??9?5%?O`ją-K?2?¸8??` ?j?I? ٵ??sS??S)`V?)<?B????&??@@ĩ???^a#5eg/?ڮb?(?M?`?#??8w??)?dT???	?"uPZ}L??#???Q??u?TF`??5%?????+??[???hA????=??j?9@cY???7'Ƹ???Q?e????[???
()	\?k?/N)?]?B/??B?	{???ܨ?s>?tuRJ?ՑNh?0?(?F??"N???'SI?@|J??-p???7?R??$i6?F?Fٻ}??P?:h??T{?p)J?Y???GB͇??R?Uj(???	͕???KGVs????>??ҍ\??$?]?? ??FB?/?c}>t??C?ք??֦$	???5?V???w?p%L??^?-?Qsh?#?F?Jr ⧗???????Y?#m?[??ϔ??]H???M?c??ArG?/?x????u?jqW?~?6)p?צ?4=?CkΚ???????nXjE?0??s?]??!B?????>/H?]{?S???????qP?9???~q?0&mV?B?Lݯ @Q???6??H)O?ђ??/?^?݌Z??$ ?h?(?F??S???ъL???*?H{??p?w???@$?R??b?ϵ?ۢ?D=~??9	M"{	???Z?v??H?6?槏rU????ar?͏J?A?8??քh@J?????5qs?RْA??;??+s%͹??܋0]??9'B<B,?j???I?? ?&w?է4,Vf??????>$??????h?tmYy???K??'(3???=???N?D?tj??3<?t?P??04˻????J"?????ƙl?oOa?6垟Sq??D-?_????b?J /N?????3?WRuT?z-ս?RiSV
w?rY??f??̽?)??????w*??????`>&)x?+p?	?Q^?????vy??7~???p,Ga??ˡ???q?/lA??I??Mi?!^?T??̠r??
????? ???;Z???K,;??-:}[űOl????%?-u???8V??7?? ??(??؊v.Ohc1GZ???dE?Kf?R8l???Z?:????KY?w?+???*k?'4???????=?8M??C\?8??pO?u??H?ӆC2???K???s8"Q;?;\?*P??ٷ??5?>???r	1?mU?8S[ڮ?,?????PhҘ?Ra[??k?-??;?k????M?C??h???5:?ݵ~4?N?ܜ?h??i?t??lJ???ҍ/???(?!;??m??/?'pk~???r??{??D??Q??A?a ?M??pD??d?Vg??a?7.w:?6i?2Q??!??͞B?ɡ?M??#"yG??)?o???????]?y4Ps?C i????M ????"&??w{?????/xP??T?X??zkꯦld;?Bf??fn~???:?w`ں?;??26????MH3E?6?X8?Y?9??"eՁM`'?s?gx.?D$?????Z?f?q??'	D?4??W'
l-?????@&?p??ꠒwg8bKW(??????$?\?ߩps?????Gy?u?mG??????Pn?7?3?+'???ya??~u?&6?? ??BΌi???p?q?x??O$?A??P?,?l? ??,i?????#|)?v??s????fX?E?MjF?Ĥ?#???L??\?p?vɎG?Uư??޹???p??i0W}??.6?????&??Tx*%?P????b???s2@???c/???&> R?x??bB?Rc?x)h?ً??U?dC??朵?R2?gMX!????s[^?n??????<?jh?
R?S_? ??%?/z?~?ib?8S?"?e?,NDQ?:?C???T?r?ͅ%=??X?e7?(I??:<???8?E?Oh?Xs?l?5?ۍ???+????wE??z2v?w?;????R???R??u3֑4&?s????P'A^	Nj??n^??t?N.?$?"?ix???? ?R?/H??lK?1Ƌ?~U?y????YۖG??^G]??x???? ??ip??:+?kF????Ff?????;v?"ϔ;?ҍ\??A????Y???H.ٵ?0????F??b'???ԯOI?CW'?/??{? ?\M?߰??d??i߁?8?9>i1??A??&?4bC?:?'%????O?I??3?e???˦qCO??f[/e??uX??[?V???e$>??l??>??Y@	\L
-?ҲM?G?G6???%??~?	??Ҁz@$?Dޡ?2??	Íڲ?{?iz,3C`%b1kh??9?o??n??V8???|jy??I??????#p?N?N?Q??ۣ+`?2 CY????0x?Źlj:??am???.?N??k?֯Oa?]$;??uԎnJB?NJV????M?? B?u_0?g?/?H"?$???- "?S?0|ȉ? mf????y????F????SF*Fb	?޳OaT?L??e!?g?????U?p?'H???C(?9ݲ?N?M???jҜ?.`m;?J?:~??Ӏu??5?~}??f?5?+??:2[????& qv	ǯ,?????S
B??f?[?q'm??TN?(-??N!???|څ????;????	|??K???	o?	??I͛?&??o?s?X	l>G1?V??Ƚ8'???Sc`??L|?HI?????B???>W_?H?V?>?`??aoe?x ????JgDH?Q??䳉k	????=ť"y???XJ???q???????f"ظ?r??T?6隮O??P????$?ˢ?I?R?]??I!5`:?,M??;er?D&2?`?|zu??8_??????v?sN???^/	)?<??B?.?6??W*G_?0ڟ?8?)????%?\΂?????"?d??EN(* }D?ӫi?Wa??7? 8??I]?D?fl?E՛?$<? ?????z??*??Ȋ;?n???n?ƒ?:???ܦy?mYO^?ϣ?w?
˓?J?%?2??X?T???????k/̬?s?s?eݹf?D????IP?I?yr????I?&~??)???2?CEC|[z??????Ħ???@??y?f??&D?rhՎN:|??ڔ|?Q??Ҝ?Zl?֬?;?9?j?H???q??
??Z?6???%?e?ѧ6??_`#N?H"?ȒX?????????p??9?1!!*L???8IA?6?????Q?O??????t8-Ri??7a??Jt.[???%\S??{8?&??XI6???M??*=?}M;?`?	?B=0?)8?>????@??jÑ??[Ҋ%?7?q?T?m?2?????j???ډ????.?V?8??f?F)OE?.??????#2{??Sy??.?$????齦Y ?8?ew?(??V0???v??m???&?????!?b?97??V?3?X?? ?~?????cdjB???? ??3?h?lȶ+?fU`????mi????&?I????&BX?C?%U<??vz.B?? eYd$?pS?w?8???)???Jx???????օCh
hF{??z
'd?Z
b?T??X?A{<????F(RC;?K!e??j?աs?=?{??[:?}?Wg$>?H????]?q?D?qB???????I???3 $??j??,???.?=?=.??j?3?0?|?C??ʁ ?z???3*?? k"??A????â?&}Z?A3`?<w)(?fI???oz7>???zƧ?[???o?W?l?/?Yݞ?]??A:?S{[???3>偍{MC1?T?:?@??p?i??P?Rn????f;?4D3?@-?z??=?6?S???s??ٳ#?<?ɻ?#?Yt?q?o?xI?ߚձ??<???}?%)[P8?   ?B?e??⽡????ս&?yq???t????S?,??䒚]?PK??nVpE???g?!???S??o? 5??f?\p?{?$??f8???}ݕ?@???(??Am-c????N???X3?-5a?????????lS???pFk_?)Qa??0?#?;9??i??y??MoАL???{?)[1????%?5
%???oj?R???i??Jق.???4T)??? 
	?Ŷ̉?Q?By??M?^??Մ	ȁ?9??]??D?=????H?X???????*?4t?gx?/ @?Z?????O$Je??????.?,??i??????p#?=E?H??2Y???[?????eC_????Ur?Q?J?????ڧYx??*zdr?
٢GS??SVhz?5?????-nޠK??7?!?k C?4?lmoV?@??w$????e?m??????C??m?k??|{ ?z?B???h/??`??!#v???fᔉ?B?F.?텎??]ar???}???Yd??<e??1????e?bY?I?K????	T?sFS[?#w???4E+?p/]8p?R??$?b???o6??+#N?;??,!?>??Dp???a??? ?
9巒`T9M????5 {???/'?b)???/?]?y?)?}R?l?n?O?Ž??] ?4??I??62y;?4(՟?6,??զ?7]Ex?KE?G?XԹ?????63??x?;{*f?+-?y?lQ???_?*?mY???~7l????yB{??}V.pAa??,e'??w?ZtcIrT??.i?W?w??Ѷ?????yy??#͔?$O?-G??????r???o???֑?#?2'??%???C?Д?n)he?-3d?d??#??Z????????@t?v_:82?ϩxr???,?ӌ?9?0????O?L$??G???m?F)?6?<e?|W?Cs?hNy#?0?3?Z?7??? Gw?????5??B/]?7??R?????Ҋt?a??I.ec"??ID?vW,?f {/q?K??<)?Ÿ?X??h?o?rh?X4a??T?7???-)?q?3Z})P??? S??[?ZQ?????X?Ӂ??TU?Ual????i?A?B???r&u?%{T?f?6D?B?	?g?߰S??zyd??5?@?,:[d۠6??ʷ?O?v-e?O?ME?~E??????(%??-??K?_$q?OQ?`??9c^?3???5v?VU:??R?????%???cr,?s?'?~?yS????c?j?????}???19???-??t???a?	?A?h??xNr* '9?)w??ՙ6??F?=???ف?+UW??6ط??p?k??<? U?????\?~?{?? Ꚑ?,OakNƞ?
??{?^R??'?X?Y?L?10R?u?Ǝ????8????>??_e?G0 ݑK5?By?4)v????ǔ??V??s^??4??<a+?\\j:??ۧ?P?nC.;?T?6?r?f???*??徰`?8?6N?????+???9?d)?s?>%?>a?????5"?3j?6????57?(??S???A??u@`_?(4ͤ?x?????>????|姵Ϟ!??v?C&?Ptɲa?I??4?p<?P?YbFK@?o? b???5?H???@G?????&??????5-??[i?:z?Nm?f@H?)???F?npO?M?xۆ)????0?I*\?'?????O??i"Y??i?????z????.????<p????OW??'?????+?Y???t??#??????	??_z???????a?4?=0?A???????S??m?vc?l'X???W?M??8~#?~??nw?0b??t?FO|??? ?ߡIG?mH?5?????mgkE?(O?jʑO!ϗ?r???5??oI??"~lM???????=h???S
??r???"N[a^?????\?i#?4??6??j??0??!???G??<?rA-????}癜?l\???"?@<?S?,\b+?nP`?Ł?}?k??S?[?k??)??????^?>g??ʱ????Sgu????2x?d\?
I??^HA??-?D???3㿝??????@     