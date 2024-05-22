cd "C:\Users\mvc32\OneDrive - University of Cambridge\Documents\Climate_meningitis_belt\Disease_data"

import excel "C:\Users\mvc32\OneDrive - University of Cambridge\Documents\Climate_meningitis_belt\Disease_data\weekly_Epid_District.xlsx", sheet("weekly_Epid_District") firstrow clear

count // Count number of responses before deduplication and filtration
tab DISTRICT
tab COUNTRY

// Standardize country names
replace COUNTRY = "Democratic Republic of the Congo" if COUNTRY == "RD Congo"
replace COUNTRY = "Chad" if COUNTRY == "Tchad"
replace COUNTRY = "Central African Republic" if COUNTRY == "Centrafrique"
replace COUNTRY = "Cameroon" if COUNTRY == "Cameroun"

// Generate combined district and country identifier
gen district_country = DISTRICT + " " + COUNTRY
gen district_country2 = DISTRICT + " " + COUNTRY

tab district_country
sort district_country

// Identify and drop duplicates
quietly by district_country: gen dup = cond(_N==1, 0, _n)
tab dup
drop if dup > 1
tab district_country

// Drop incorrect or unwanted entries
drop if district_country in (" TanguiÚta Benin", "BogandT Burkina Faso", "BoussÚ Burkina Faso", "BoussÚ P¶ Burkina Faso", "SaponT Burkina Faso")
sort district_country

save "weeklyincidence.dta", replace

// Correct district_country names
replace district_country = "Addis Abeba Ethiopia" if district_country == "Bole 5 Ethiopia"
replace district_country = "Nouadhibou Mauritania" if district_country == "Chami Mauritania"
replace district_country = "Addis Ababa Ethiopia" if district_country == "Hakim Ethiopia"
replace district_country = "Banikoara Benin" if district_country == "BANIKOARA Benin"
replace district_country = "Gedeo Ethiopia" if district_country == "Birbirsa Kojowa Ethiopia"
replace district_country = "Guji Ethiopia" if district_country == "Dima Ethiopia"
replace district_country = "Gambella Town Ethiopia" if district_country == "Gambella Woreda Ethiopia"
replace district_country = "Mambasa Democratic Republic of the Congo" if district_country == "MAMBASSA Democratic Republic of the Congo"
replace district_country = "Zongo Democratic Republic of the Congo" if district_country == "ZONGO Democratic Republic of the Congo"
replace district_country = "Yalanga Democratic Republic of the Congo" if district_country == "YALANGA Democratic Republic of the Congo"
replace district_country = "Yamaluka Democratic Republic of the Congo" if district_country == "YAMALUKA Democratic Republic of the Congo"
replace district_country = "Tshofa Democratic Republic of the Congo" if district_country == "TSHOFA Democratic Republic of the Congo"
replace district_country = "Tshopo Democratic Republic of the Congo" if district_country == "TSHOPO Democratic Republic of the Congo"
replace district_country = "Vaku Democratic Republic of the Congo" if district_country == "VAKU Democratic Republic of the Congo"
replace district_country = "Wikong Democratic Republic of the Congo" if district_country == "WIKONG Democratic Republic of the Congo"
replace district_country = "Pimu Democratic Republic of the Congo" if district_country == "PIMU Democratic Republic of the Congo"
replace district_country = "Panda Democratic Republic of the Congo" if district_country == "PANDA Democratic Republic of the Congo"
replace district_country = "Salamabila Democratic Republic of the Congo" if district_country == "SARAMBILA Democratic Republic of the Congo"
replace district_country = "Baka Democratic Republic of the Congo" if district_country == "BAKA Democratic Republic of the Congo"
replace district_country = "Bangabola Democratic Republic of the Congo" if district_country == "BANGABOLA Democratic Republic of the Congo"
replace district_country = "Basali Democratic Republic of the Congo" if district_country == "BASALI Democratic Republic of the Congo"
replace district_country = "Bibanga Democratic Republic of the Congo" if district_country == "BIBANGA Democratic Republic of the Congo"
replace district_country = "Bili Democratic Republic of the Congo" if district_country == "BILI Democratic Republic of the Congo"
replace district_country = "Binga Democratic Republic of the Congo" if district_country == "BINGA Democratic Republic of the Congo"
replace district_country = "Bongandanga Democratic Republic of the Congo" if district_country == "BONGANDANGA Democratic Republic of the Congo"
replace district_country = "Bosondjo Democratic Republic of the Congo" if district_country == "BOSONDJO Democratic Republic of the Congo"
replace district_country = "Boto Democratic Republic of the Congo" if district_country == "BOTO Democratic Republic of the Congo"
replace district_country = "Bulape Democratic Republic of the Congo" if district_country == "BULAPE Democratic Republic of the Congo"
replace district_country = "Bulu Democratic Republic of the Congo" if district_country == "BULU Democratic Republic of the Congo"
replace district_country = "Bunkeya Democratic Republic of the Congo" if district_country == "BUNKEYA Democratic Republic of the Congo"
replace district_country = "Bunkonde Democratic Republic of the Congo" if district_country == "BUNKONDE Democratic Republic of the Congo"
replace district_country = "Bafwagbogbo Democratic Republic of the Congo" if district_country == "Bafwabgobgo Democratic Republic of the Congo"
replace district_country = "Damas Democratic Republic of the Congo" if district_country == "DAMAS Democratic Republic of the Congo"
replace district_country = "Dikungu Democratic Republic of the Congo" if district_country == "Dikungu Tshumbe Democratic Republic of the Congo"
replace district_country = "Doruma Democratic Republic of the Congo" if district_country == "Doruma Democratic Republic of the Congo"
replace district_country = "Fataki Democratic Republic of the Congo" if district_country == "FATAKI Democratic Republic of the Congo"
replace district_country = "Gombari Democratic Republic of the Congo" if district_country == "GOMBARI Democratic Republic of the Congo"
replace district_country = "Gethy Democratic Republic of the Congo" if district_country == "Gethy Democratic Republic of the Congo"
replace district_country = "Aguié Niger" if district_country == "Aguié Nige"
replace district_country = "Addis Abeba Ethiopia" if district_country == "Akaki 12 Ethiopia"
replace district_country = "Addis Abeba Ethiopia" if district_country == "Akaki 13 Ethiopia"
replace district_country = "Aleiro Nigeria" if district_country == "Aliero Nigeria"
replace district_country = "Iboko Democratic Republic of the Congo" if district_country == "IBOKO Democratic Republic of the Congo"
replace district_country = "Itebero Democratic Republic of the Congo" if district_country == "ITEBERO Democratic Republic of the Congo"
replace district_country = "Itombwe Democratic Republic of the Congo" if district_country == "ITOMBWE Democratic Republic of the Congo"
replace district_country = "Kabondo-Dianda Democratic Republic of the Congo" if district_country == "KABONDO-DIANDA Democratic Republic of the Congo"
replace district_country = "Kakenge Democratic Republic of the Congo" if district_country == "KAKENGE Democratic Republic of the Congo"
replace district_country = "Kapolowe Democratic Republic of the Congo" if district_country == "KAPOLOWE Democratic Republic of the Congo"
replace district_country = "Kibombo Democratic Republic of the Congo" if district_country == "KIBOMBO Democratic Republic of the Congo"
replace district_country = "Kilwa Democratic Republic of the Congo" if district_country == "KILWA Democratic Republic of the Congo"
replace district_country = "Kinkondja Democratic Republic of the Congo" if district_country == "KINKONDJA Democratic Republic of the Congo"
replace district_country = "Kinkonzi Democratic Republic of the Congo" if district_country == "KINKONZI Democratic Republic of the Congo"
replace district_country = "Kungu Democratic Republic of the Congo" if district_country == "KUNGU Democratic Republic of the Congo"
replace district_country = "Lilanga Bobangi Democratic Republic of the Congo"  if district_country == "LILANGA-BOBANGI Democratic Republic of the Congo"
replace district_country = "Limete Democratic Republic of the Congo"  if district_country == "LIMETE Democratic Republic of the Congo"
replace district_country = "Lita Democratic Republic of the Congo"  if district_country == "LITA Democratic Republic of the Congo"
replace district_country = "Lualaba Democratic Republic of the Congo"  if district_country == "LUALABA Democratic Republic of the Congo"
replace district_country = "Lubudi Democratic Republic of the Congo"  if district_country == "LUBUDI Democratic Republic of the Congo"
replace district_country = "Ludimbi-Lukula Democratic Republic of the Congo"  if district_country == "LUDIMBI-LUKULA Democratic Republic of the Congo"
replace district_country = "Lukolela Democratic Republic of the Congo"  if district_country == "LUKOLELA Democratic Republic of the Congo"
replace district_country = "Luputa Democratic Republic of the Congo"  if district_country == "LUPUTA Democratic Republic of the Congo"
replace district_country = "Mangobo Democratic Republic of the Congo"  if district_country == "MANGOBO Democratic Republic of the Congo"
replace district_country = "Mulongo Democratic Republic of the Congo"  if district_country == "MULONGO Democratic Republic of the Congo"
replace district_country = "Mutoto Democratic Republic of the Congo"  if district_country == "MUTOTO Democratic Republic of the Congo"
replace district_country = "Ndage Democratic Republic of the Congo"  if district_country == "NDAGE Democratic Republic of the Congo"
replace district_country = "Nizi Democratic Republic of the Congo"  if district_country == "NIZI Democratic Republic of the Congo"
replace district_country = "Kalonge Democratic Republic of the Congo"  if district_country == "Kalonge Democratic Republic of the Congo"
replace district_country = "Katako-Kombe Democratic Republic of the Congo"  if district_country == "Katoka Democratic Republic of the Congo"
replace district_country = "Kilo Democratic Republic of the Congo"  if district_country == "Kilo Democratic Republic of the Congo"
replace district_country = "Lolwa Democratic Republic of the Congo"  if district_country == "Lolwa Democratic Republic of the Congo"
replace district_country = "Luambo Democratic Republic of the Congo"  if district_country == "Luambo Democratic Republic of the Congo"
replace district_country = "Mompono Democratic Republic of the Congo"  if district_country == "Mompono Democratic Republic of the Congo"
replace district_country = "Monga Democratic Republic of the Congo"  if district_country == "Monga Democratic Republic of the Congo"
replace district_country = "Mosango Democratic Republic of the Congo"  if district_country == "Mosango Democratic Republic of the Congo"
replace district_country = "Mulongo Democratic Republic of the Congo"  if district_country == "Mulongo Democratic Republic of the Congo"
replace district_country = "Mushenge Democratic Republic of the Congo"  if district_country == "Mushenge Democratic Republic of the Congo"
replace district_country = "Ndjoko Punda Democratic Republic of the Congo"  if district_country == "Ndjokopunda Democratic Republic of the Congo"
replace district_country = "Obokote Democratic Republic of the Congo"  if district_country == "OBOKOTE Democratic Republic of the Congo"
replace district_country = "Opienge Democratic Republic of the Congo"  if district_country == "Opienge Democratic Republic of the Congo"
replace district_country = "Panzi Democratic Republic of the Congo"  if district_country == "Panzi Democratic Republic of the Congo"
replace district_country = "Pawa Democratic Republic of the Congo"  if district_country == "Pawa Democratic Republic of the Congo"
replace district_country = "Songa Democratic Republic of the Congo"  if district_country == "Songa Democratic Republic of the Congo"
replace district_country = "Tchomia Democratic Republic of the Congo"  if district_country == "Tchomia Democratic Republic of the Congo"
replace district_country = "Wanie-Rukula Democratic Republic of the Congo"  if district_country == "Wanie Rukula Democratic Republic of the Congo"
replace district_country = "Wasolo Democratic Republic of the Congo"  if district_country == "Wasolo Democratic Republic of the Congo"
replace district_country = "Kembe-Satema Democratic Republic of the Congo"  if district_country == "Kémbé-Satéma Central African Republic"
replace district_country = "Djougou Benin"  if district_country == "Djougou Rural"
replace district_country = "Ouaké Benin"  if district_country == "Ouakþ Benin"
replace district_country = "Didievi Côte D'Ivoire"  if district_country == "Didievi Cote d'Ivoire"
replace district_country = "Sakassou Côte D'Ivoire"  if district_country == "Sakassou Cote d'Ivoire"
replace district_country = "Afar Zone 1 Ethiopia"  if district_country == "Aysa-ita woreda Ethiopia"
replace district_country = "Afar Zone 1 Ethiopia"  if district_country == "Aysaeta Ethiopia"
replace district_country = "Addis Ababa Ethiopia"  if district_country == "Bole 5 Ethiopia"
replace district_country = "West Harerghe Ethiopia"  if district_country == "Chiro Town Ethiopia"
replace district_country = "Kollo Niger"  if district_country == "DOUNGAS Niger"
replace district_country = "Gode Ethiopia" if district_country == "Danan Ethiopia"
replace district_country = "East Hararge Ethiopia" if district_country == "Deder Town Ethiopia"
replace district_country = "Jarar Ethiopia" if district_country == "Degahbur Ethiopia"
replace district_country = "Jarar Ethiopia" if district_country == "Degehabur Rural Ethiopia"
replace district_country = "Afar Zone 1 Ethiopia" if district_country == "Dubti city Ethiopia"
replace district_country = "Semen Gondar Ethiopia" if district_country == "East Belesa Ethiopia"
replace district_country = "Liben Ethiopia" if district_country == "Filtu Ethiopia"
replace district_country = "Gambela Ethiopia" if district_country == "Gambella Woreda Ethiopia"
replace district_country = "Bale Ethiopia" if district_country == "GOBA TOWN Ethiopia"
replace district_country = "Bale Ethiopia" if district_country == "Ginnir Town Ethiopia"
replace district_country = "Bale Ethiopia" if district_country == "Goba-Town Ethiopia"
replace district_country = "Addis Ababa Ethiopia"  if district_country == "Hakim Ethiopia"
replace district_country = "Jijiga Ethiopia"  if district_country == "Jijga City Ethiopia"
replace district_country = "South Omo Ethiopia"  if district_country == "Jikaw Ethiopia"
replace district_country = "Asosa Ethiopia"  if district_country == "Menge Ethiopia"
replace district_country = "Bench Maji Ethiopia"  if district_country == "Mizan Ethiopia"
replace district_country = "Afder Ethiopia"  if district_country == "Raso Ethiopia"
replace district_country = "South Wollo Ethiopia"  if district_country == "Sayint Ethiopia"
replace district_country = "Fafan Ethiopia"  if district_country == "Tuliguled Ethiopia"
replace district_country = "Gedeo Ethiopia"  if district_country == "Wenago Ethiopia"
replace district_country = "Central Tigray Ethiopia"  if district_country == "Werie lehe Ethiopia"
replace district_country = "Silte Ethiopia"  if district_country == "Worabie TA Ethiopia"
replace district_country = "Borena Ethiopia"  if district_country == "Yabello T Ethiopia"
replace district_country = "Borena Ethiopia"  if district_country == "Yabelo Ethiopia"
replace district_country = "Gedeo Ethiopia"  if district_country == "Yirga Chefe Town Ethiopia"
replace district_country = "Hareri Ethiopia"  if district_country == "Amir-nur Ethiopia"
replace district_country = "Aweil Central South Sudan"  if district_country == "Awiel Centre South Sudan"
replace district_country = "Guelendeng Chad"  if district_country == "Gueledeng Chad"
replace district_country = "Aweil Central South Sudan"  if district_country == "Awiel Centre South Sudan"
replace district_country = "Ndjamena Est Chad"  if district_country == "Ndjamena-Est Chad"
replace district_country = "Kabbia Chad"  if district_country == "Kabbia Chad"
replace district_country = "Kasena Nankana East Ghana"  if district_country == "Kassena-nankana Ghana"
replace district_country = "Maradi Ville Niger"  if district_country == "Maradi Niger"
replace district_country = "Nongr Massom Burkina Faso"  if district_country == "Nongre-Massom Burkina Faso"
replace district_country = "Tidermene Mali"  if district_country == "Tindermen Mali"
replace district_country = "Boukoumbé Benin" if district_country == "BOUKOUMBE Benin"
 replace district_country = "Cobly Benin" if district_country == "COBLY Benin"
 replace district_country = "Matéri Benin" if district_country == "MATERI Benin"
 replace district_country = "N'Dali Benin" if district_country =="N'dali Benin"
 replace district_country = "Natitingou Benin" if district_country == "NATITINGOU Benin"
 replace district_country = "Nikki Benin" if district_country == "NIKKI Benin"
 replace district_country = "Pèrèrè Benin" if district_country =="PERERE Benin"
 replace district_country = "Tanguiéta Benin" if district_country =="TANGUIETA Benin"
 replace district_country = "Toucountouna Benin" if district_country =="TOUCOUNTOUNA Benin"
 replace district_country = "Tanguiéta Benin" if district_country =="TanguiÚta Benin"
replace district_country = "Océan Cameroon" if district_country == "Kribi Cameroon"
replace district_country = "Océan Cameroon" if district_country == "Lolodorf Cameroon"
replace district_country = "Bougouriba Burkina Faso" if district_country == "DiÚbougou Burkina Faso"
replace district_country = "Moungo Cameroon" if district_country == "Loum Cameroon"
replace district_country = "Bougouriba Burkina Faso" if district_country == "Diébougou Burkina Faso"
replace district_country = "Sissili Burkina Faso" if district_country == "Léo Burkina Faso"
replace district_country = "Nahouri Burkina Faso" if district_country == "Pô Burkina Faso"
replace district_country = "Boulgou Burkina Faso" if district_country == "ZabrÚ Burkina Faso"
replace district_country = "Bénoué Cameroon" if district_country == "Lagdo Cameroon"
replace district_country = "Bénoué Cameroon" if district_country == "Bibémi Cameroon"
replace district_country = "Bénoué Cameroon" if district_country == "Pitoa Cameroon"
replace district_country = "Moungo Cameroon" if district_country == "Dibombari Cameroon"
replace district_country = "Haut Nyong Cameroon" if district_country == "Doumé Cameroon"
replace district_country = "Mefou et Afamba Cameroon" if district_country == "Esse Cameroon"
replace district_country = "Meme Cameroon" if district_country == "Konye Cameroon"
replace district_country = "Diamaré Cameroon" if district_country == "Maroua Rural Cameroon"
replace district_country = "Faro Cameroon" if district_country == "Poli Cameroon"
replace district_country = "Noun Cameroon" if district_country == "Massangam Cameroon"
replace district_country = "Mayo Tsanaga Cameroon" if district_country == "Mokolo Cameroon"
replace district_country = "Lekié Cameroon" if district_country == "Monatélé Cameroon"
replace district_country = "Boumba et Ngoko Cameroon"  if district_country == "Mouloundou Cameroon"
replace district_country = "Mayo Kani Cameroon" if district_country == "Moutourwa Cameroon"
replace district_country = "Mvila Cameroon" if district_country == "Mvangan Cameroon"
replace district_country = "Wouri Cameroon" if district_country == "New Bell Cameroon"
replace district_country = "Mefou et Akono Cameroon" if district_country == "Ngoumou Cameroon"
replace district_country = "Koupé Manengouba Cameroon" if district_country == "Nguti Cameroon"
replace district_country = "Momo Cameroon" if district_country == "Njikwa Cameroon"
replace district_country = "Moungo Cameroon" if district_country == "Njombe Penja Cameroon"
replace district_country = "Sanaga Maritime Cameroon" if district_country == "Pouma Cameroon"
replace district_country = "Koupé Manengouba Cameroon" if district_country == "Tombel Cameroon"
replace district_country = "Dja et Lobo Cameroon" if district_country == "Zoétélé Cameroon"
replace district_country = "Bozoum Central African Republic" if district_country == "BOZOUM-BOSSEMPTELE Central African Republic"
replace district_country = "Birnin-Magaji/Kiyaw Nigeria" if district_country == "Birnin Magaji Nigeria"
replace district_country = "Birnin-Magaji/Kiyaw Nigeria" if district_country == "Birnin Magaji/Kiyawa Nigeria"
replace district_country = "Birnin Kudu Nigeria" if district_country == "Birnin-Kudu Nigeria"
replace district_country = "Biriniwa Nigeria" if district_country == "Birniwa Nigeria"
replace district_country = "Borsari Nigeria" if district_country == "Bursari Nigeria"
replace district_country = "Dutsinma Nigeria" if district_country == "Dutsin Ma Nigeria"
replace district_country = "Itas/Gadau Nigeria" if district_country == "Itas-Gadau Nigeria"
replace district_country = "Jama'are Nigeria" if district_country == "Jama'Are Nigeria"
replace district_country = "Jibia Nigeria" if district_country == "Jibya Nigeria"
replace district_country = "Koko/Besse Nigeria" if district_country == "Koko Bese Nigeria"
replace district_country = "Mai'Adua Nigeria" if district_country == "Maiaduwa Nigeria"
replace district_country = "Malam Maduri Nigeria" if district_country == "Malam-Maduri Nigeria"
replace district_country = "Malumfashi Nigeria" if district_country == "Malunfashi Nigeria"
replace district_country = "Shomgom Nigeria" if district_country == "Shongom Nigeria"
replace district_country = "Sule Tankarkar Nigeria" if district_country == "Sule Tankakar Nigeria"
replace district_country = "Sule Tankarkar Nigeria" if district_country == "Sule-Tankarkar Nigeria"
replace district_country = "Talata-Mafara Nigeria" if district_country == "Talata Mafara Nigeria"
replace district_country = "Yamaltu/Deba Nigeria" if district_country == "Yamaltu Deba Nigeria"
replace district_country = "Niamey Niger" if district_country == "Niamey 1 Niger"
replace district_country = "Niamey Niger" if district_country == "Niamey 2 Niger"
replace district_country = "Niamey Niger" if district_country == "Niamey 3 Niger"
replace district_country = "Niamey Niger" if district_country == "Niamey 4 Niger"
replace district_country = "Aguié Niger" if district_country == "AguiÚ Niger"
replace district_country = "Niamey Niger" if district_country == "Niamey 5 Niger"
replace district_country = "Aguié Niger" if district_country == "AguiÚ Niger"
replace district_country = "Bkonni Niger" if district_country == "Birni N'Konni Niger"
replace district_country = "Dosso Niger" if district_country == "DIOUNDIOU Niger"
replace district_country = "Filingué Niger" if district_country == "Filingue Niger"
replace district_country = "Gaya Niger" if district_country == "Gaya Niger"
replace district_country = "Gouré Niger" if district_country == "Goure Niger"
replace district_country = "Illéla Niger" if district_country == "IllÚla Niger"
replace district_country = "Illéla Niger" if district_country == "Illela Niger"
replace district_country = "Matameye Niger" if district_country == "MatamÞye Niger"
replace district_country = "N'Guigmi Niger" if district_country == "N'guigmi Niger"
replace district_country = "Groumdji Niger" if district_country == "TIBIRI Niger"
replace district_country = "Téra Niger" if district_country == "Tera Niger"
replace district_country = "Bimah Togo" if district_country=="Binah Togo"
replace district_country = "Lomé Togo" if district_country=="District V Togo"
replace district_country = "Kéran Togo" if district_country=="Keran Togo"
replace district_country = "Comoé Burkina Faso" if DISTRICT == "Banfora"
replace district_country = "Sanmatenga Burkina Faso" if DISTRICT == "Barsalogo"
replace district_country = "Noumbiel Burkina Faso" if DISTRICT == "Batié"
replace district_country = "Boulgou Burkina Faso" if DISTRICT == "Bittou"
replace district_country = "Gnagna Burkina Faso" if DISTRICT == "BogandÚ"
replace district_country = "Balé Burkina Faso" if DISTRICT == "Boromo"
replace district_country = "Namentenga Burkina Faso" if DISTRICT == "Boulsa"
replace district_country = "Kourwéogo Burkina Faso" if DISTRICT == "Boussé"
replace district_country = "Houet Burkina Faso" if DISTRICT == "Dandé"
replace district_country = "Ioba Burkina Faso" if DISTRICT == "Dano"
replace district_country = "Bougouriba Burkina Faso" if DISTRICT == "Dano"
replace district_country = "Soum Burkina Faso" if DISTRICT == "Djibo"
replace district_country = "Séno Burkina Faso" if DISTRICT == "Dori"
replace district_country = "Mouhoun Burkina Faso" if DISTRICT == "Dédougou"
replace district_country = "Gourma Burkina Faso" if DISTRICT == "Fada N'gourma"
replace district_country = "Poni Burkina Faso" if DISTRICT == "Gaoua"
replace district_country = "Boulgou Burkina Faso" if DISTRICT == "Garango"
replace district_country = "Komandjoari Burkina Faso" if DISTRICT == "Gayeri"
replace district_country = "Oudalan Burkina Faso" if DISTRICT == "Gorom-Gorom"
replace district_country = "Zondoma Burkina Faso" if DISTRICT == "Gourcy"
replace district_country = "Tuy Burkina Faso" if DISTRICT == "Houndé"
replace district_country = "Houet Burkina Faso" if DISTRICT == "Karangasso-ViguT"
replace district_country = "Sanmatenga Burkina Faso" if DISTRICT == "Kaya"
replace district_country = "Bazèga Burkina Faso" if DISTRICT == "Kombissiri"
replace district_country = "Bam Burkina Faso" if DISTRICT == "Kongoussi"
replace district_country = "Boulkiemdé Burkina Faso" if DISTRICT == "Koudougou"
replace district_country = "Kouritenga Burkina Faso" if DISTRICT == "Koupela"
replace district_country = "Houet Burkina Faso" if DISTRICT == "Lena"
replace district_country = "Sissili Burkina Faso" if DISTRICT == "LÚo"
replace district_country = "Comoé Burkina Faso" if DISTRICT == "Mangodara"
replace district_country = "Zoundwéogo Burkina Faso" if DISTRICT == "Manga"
replace district_country = "Kénédougou Burkina Faso" if DISTRICT == "N' Dorola"
replace district_country = "Boulkiemdé Burkina Faso" if DISTRICT == "Nanoro"
replace district_country = "Kossi Burkina Faso" if DISTRICT == "Nouna"
replace district_country = "Kénédougou Burkina Faso" if DISTRICT == "Orodara"
replace district_country = "Yatenga Burkina Faso" if DISTRICT == "Ouahigouya"
replace district_country = "Koulpélogo Burkina Faso" if DISTRICT == "Ouargaye"
replace district_country = "Kompienga Burkina Faso" if DISTRICT == "Pama"
replace district_country = "Kouritenga Burkina Faso" if DISTRICT == "Pouytenga"
replace district_country = "Nahouri Burkina Faso" if DISTRICT == "P¶"
replace district_country = "Sanguié Burkina Faso" if DISTRICT == "Réo"
replace district_country = "Bazèga Burkina Faso" if DISTRICT == "SaponÚ"
replace district_country = "Ziro Burkina Faso" if DISTRICT == "Sapouy"
replace district_country = "Yagha Burkina Faso" if DISTRICT == "Sebba"
replace district_country = "Yatenga Burkina Faso" if DISTRICT == "Seguenega"
replace district_country = "Boulgou Burkina Faso" if DISTRICT == "Sig-Nonghin"
replace district_country = "Léraba Burkina Faso" if DISTRICT == "Sindou"
replace district_country = "Banwa Burkina Faso" if DISTRICT == "Solenzo"
replace district_country = "Boulgou Burkina Faso" if DISTRICT == "Tenkodogo"
replace district_country = "Loroum Burkina Faso" if DISTRICT == "Titao"
replace district_country = "Nayala Burkina Faso" if DISTRICT == "Toma"
replace district_country = "Sourou Burkina Faso" if DISTRICT == "Tougan"
replace district_country = "Passoré Burkina Faso" if DISTRICT == "Yako"
replace district_country = "Boulgou Burkina Faso" if DISTRICT == "Yako"
replace district_country = "Boulgou Burkina Faso" if DISTRICT == "Zabré"
replace district_country = "Oubritenga Burkina Faso" if DISTRICT == "ZiniarT"
replace district_country = "Ganzourgou Burkina Faso" if DISTRICT == "Zorgho"
replace district_country = "Ganzourgou Burkina Faso" if DISTRICT == "Zorgho"
replace district_country = "Ngo Ketunjia Cameroon" if DISTRICT == "Bangourain"
replace district_country = "Mayo Louti Cameroon" if DISTRICT == "Figuil"
replace district_country = "Boyo Cameroon" if DISTRICT == "Fundong"
replace district_country = "Mayo Tsanaga Cameroon" if DISTRICT == "Hina"
replace district_country = "Mayo Kani Cameroon" if DISTRICT == "Kaélé"
replace district_country = "Haute Sanaga Cameroon" if DISTRICT == "Mbandjock"
replace district_country = "Mayo Kani Cameroon" if DISTRICT == "Mindif"
replace district_country = "Gambo-Ouango Central African Republic" if DISTRICT == "Ouango-Gambo"
replace district_country = "Gambo-Ouango Central African Republic" if DISTRICT == "Mindif"
replace district_country = "Paoua Central African Republic" if DISTRICT == "PAOUA"
replace district_country = "Ouara Chad" if DISTRICT == "Abeche"
replace district_country = "Djourf Al Ahmar Chad" if DISTRICT == "Am Dam"
replace district_country = "Barh Azoum Chad" if DISTRICT == "Am Timan"
replace district_country = "Nya Pendé Chad" if DISTRICT == "Bebedjia"
replace district_country = "Ngourkosso Chad" if DISTRICT == "Benoye"
replace district_country = "Tandjilé Ouest Chad" if DISTRICT == "Bere"
replace district_country = "Tandjilé Ouest Chad" if DISTRICT == "Dono Manga"
replace district_country = "Ennedi Ouest Chad" if DISTRICT == "Fada"
replace district_country = "Mandoul Occidental Chad" if DISTRICT == "Goundi"
replace district_country = "Kabbia Chad	" if DISTRICT == "Gaya"
replace district_country = "Tandjilé Ouest Chad" if DISTRICT == "Kelo"
replace district_country = "Gontougo Côte d'Ivoire" if DISTRICT == "BONDOUKOU"
replace district_country = "Bounkani Côte d'Ivoire" if DISTRICT == "BOUNA"
replace district_country = "Bounkani Côte d'Ivoire" if DISTRICT == "Bouna"
replace district_country = "Bounkani Côte d'Ivoire" if DISTRICT == "NASSIAN"
replace district_country = "Bagoué Côte d'Ivoire" if DISTRICT == "TENGRELA"
replace district_country = "Agnuak Ethiopia" if DISTRICT == "Abobo"
replace district_country = "Ngourkosso Chad" if DISTRICT == "Benoye"
replace district_country = "Jimma Ethiopia" if DISTRICT == "Agaro town"
replace district_country = "Nuer Ethiopia" if DISTRICT == "Akobo"
replace district_country = "Oromia Ethiopia" if DISTRICT == "Amigna"
replace district_country = "Sikasso Mali" if district_country =="Sélingué Mali"
replace district_country = "Agew Awi Ethiopia" if DISTRICT == "Banja"
replace district_country = "Oromia Ethiopia" if DISTRICT == "Bekoji"
replace district_country = "Semen Gondar Ethiopia" if DISTRICT == "Beyeda"
replace district_country = "Bounkani Côte d'Ivoire" if DISTRICT == "Bouna"
replace district_country = "Bounkani Côte d'Ivoire" if DISTRICT == "NASSIAN"
replace district_country = "Bagoué Côte d'Ivoire" if DISTRICT == "TENGRELA"
replace district_country = "Agnuak Ethiopia" if DISTRICT == "Abobo"
replace district_country = "Ngourkosso Chad" if DISTRICT == "Benoye"
replace district_country = "Jimma Ethiopia" if DISTRICT == "Agaro town"
replace district_country = "Nuer Ethiopia" if DISTRICT == "Akobo"
replace district_country = "Oromia Ethiopia" if DISTRICT == "Bule Hora T"
replace district_country = "Kati Mali" if district_country == "Ouélessébougou Mali"
replace district_country = "Ho Municipal Ghana" if district_country == "Ho Ghana"
replace district_country = "East Mamprusi Ghana" if district_country == "East mamprusi Ghana"
replace district_country = "Sunyani Ghana" if district_country == "Sunyani Municipal Ghana"
replace district_country = "Foni Bintang Karanai Gambia" if district_country == "Foni Bintang-Karenai Gambia"
replace district_country = "Janjanbureh Gambia" if district_country == "Maccarthy Island (Janjanbureh) Gambia"
replace district_country = "Aboudeïa Chad" if district_country == "Abou deia Chad"
replace district_country = "Grande Sido Chad" if district_country == "Danamadji Chad"
replace district_country = "Pendé Chad" if district_country == "Doba Chad"
replace district_country = "Mont Illi Chad" if district_country == "Fianga Chad"
replace district_country = "Kabbia Chad" if district_country == "Gounou Gaya Chad"
replace district_country = "Mandoul Oriental Chad" if district_country == "Koumra Chad"
replace district_country = "Lac Iro Chad" if district_country == "Kyabe Chad"
replace district_country = "Tandjilé Est Chad" if district_country == "Lai Chad"
replace district_country = "Lac Léré Chad" if district_country == "Lere Chad"
replace district_country = "Guéra Chad" if district_country == "Mongo Chad"
replace district_country = "Guéra Chad" if district_country == "Melfi Chad"
replace district_country = "Barh Sara Chad" if district_country == "Moissala Chad"
replace district_country = "Lac Wey Chad" if district_country == "Moundou Chad"
replace district_country = "Barh Köh Chad" if district_country == "Sarh Chad"
replace district_country = "Tibesti Chad" if district_country == "Zouar Chad"
replace district_country = "Aba Democratic Republic of the Congo" if district_country == "ABA Democratic Republic of the Congo"
replace district_country = "Kindu Democratic Republic of the Congo" if district_country == "ALUNGULI Democratic Republic of the Congo"
replace district_country = "Kindu Democratic Republic of the Congo" if district_country == "Alunguli Democratic Republic of the Congo"
replace district_country = "Mbandaka Democratic Republic of the Congo" if district_country == "Wangata Democratic Republic of the Congo" 
replace district_country = "Kindu Democratic Republic of the Congo" if district_country == "Alunguli Democratic Republic of the Congo" 
replace district_country = "Banalia Democratic Republic of the Congo" if district_country == "BANALIA Democratic Republic of the Congo"
replace district_country = "Bikoro Democratic Republic of the Congo" if district_country == "BIKORO Democratic Republic of the Congo"
replace district_country = "Bondo (ville) Democratic Republic of the Congo" if district_country == "BONDO Democratic Republic of the Congo"
replace district_country = "Budjala Democratic Republic of the Congo" if district_country == "BUDJALA Democratic Republic of the Congo"
replace district_country = "Bunia Democratic Republic of the Congo" if district_country == "BUNIA Democratic Republic of the Congo"
replace district_country = "Buta (ville) Democratic Republic of the Congo" if district_country == "BUTA Democratic Republic of the Congo"
replace district_country = "Bumba (ville) Democratic Republic of the Congo" if district_country == "Bambu Democratic Republic of the Congo"
replace district_country = "Dekese Democratic Republic of the Congo" if district_country == "DEKESE Democratic Republic of the Congo"
replace district_country = "Faradje Democratic Republic of the Congo" if district_country == "FARADJE Democratic Republic of the Congo"
replace district_country = "Gbadolite Democratic Republic of the Congo" if district_country == "GBADOLITE Democratic Republic of the Congo"
replace district_country = "Kinshasa Democratic Republic of the Congo" if district_country == "KINTAMBO Democratic Republic of the Congo"
replace district_country = "Kinshasa Democratic Republic of the Congo" if district_country == "GOMBE Democratic Republic of the Congo"
replace district_country = "Kinshasa Democratic Republic of the Congo" if district_country == "Gombe Democratic Republic of the Congo"
replace district_country = "Bukavu Democratic Republic of the Congo" if district_country == "IBANDA Democratic Republic of the Congo"
replace district_country = "Kabambare Democratic Republic of the Congo" if district_country == "KABAMBARE Democratic Republic of the Congo"
replace district_country = "Kabinda Democratic Republic of the Congo" if district_country == "KABINDA Democratic Republic of the Congo"
replace district_country = "Pangi Democratic Republic of the Congo" if district_country == "Kampene Democratic Republic of the Congo"
replace district_country = "Lubumbashi Democratic Republic of the Congo" if district_country == "Katuba Democratic Republic of the Congo"
replace district_country = "Kinshasa Democratic Republic of the Congo" if district_country == "Kingasani Democratic Republic of the Congo"
replace district_country = "Kinshasa Democratic Republic of the Congo" if district_country == "LIMETE Democratic Republic of the Congo"
replace district_country = "Aketi Democratic Republic of the Congo" if district_country == "LIKATI Democratic Republic of the Congo"
replace district_country = "Lubutu (ville) Democratic Republic of the Congo" if district_country == "LUBUTU Democratic Republic of the Congo"
replace district_country = "Aketi Democratic Republic of the Congo" if district_country == "Likati Democratic Republic of the Congo"
replace district_country = "Kinshasa Democratic Republic of the Congo" if district_country == "Lingwala Democratic Republic of the Congo"
replace district_country = "Makanza Democratic Republic of the Congo" if district_country == "MAKANZA Democratic Republic of the Congo"
replace district_country = "Kolwezi Democratic Republic of the Congo" if district_country == "MANIKA Democratic Republic of the Congo"
replace district_country = "Manono (ville) Democratic Republic of the Congo" if district_country == "MANONO Democratic Republic of the Congo"
replace district_country = "Fizi Democratic Republic of the Congo" if district_country == "MINEMBWE Democratic Republic of the Congo"
replace district_country = "Bolomba Democratic Republic of the Congo" if district_country == "MONIEKA Democratic Republic of the Congo"
replace district_country = "Mweka Democratic Republic of the Congo" if district_country == "MWEKA Democratic Republic of the Congo"
replace district_country = "Mambasa Democratic Republic of the Congo" if district_country == "NIA-NIA Democratic Republic of the Congo"
replace district_country = "Mambasa Democratic Republic of the Congo" if district_country == "Niania Democratic Republic of the Congo"
replace district_country = "Pweto Democratic Republic of the Congo" if district_country == "PWETO Democratic Republic of the Congo"
replace district_country = "Hareri Ethiopia" if district_country =="Jenela Ethiopia" 
replace district_country = "Hareri Ethiopia" if district_country =="Jinela Ethiopia" 
replace district_country = "Doolo Ethiopia" if district_country =="Lehel-Yucub Ethiopia" 
replace district_country = "Ubundu Democratic Republic of the Congo" if district_country == "UBUNDU Democratic Republic of the Congo"
replace district_country = "Walikale Democratic Republic of the Congo" if district_country == "WALIKALE Democratic Republic of the Congo"
replace district_country = "Doolo Ethiopia" if district_country =="Galhamur Ethiopia"
replace district_country = "Mbandaka Democratic Republic of the Congo" if district_country == "WANGATA Democratic Republic of the Congo"
replace district_country = "Jarar Ethiopia" if district_country =="Kabribayah Town Ethiopia"
replace district_country = "Guji Ethiopia" if DISTRICT == "Uraga"
replace district_country = "North Shewa Ethiopia" if DISTRICT == "Debrebirhan Town"
replace district_country = "Tahoua Niger" if DISTRICT == "Abalak"
replace district_country = "Tchighozerine Niger" if DISTRICT == "Agadez"
replace district_country = "Aguié Nige" if DISTRICT == "AguiÚ"
replace district_country = "Tillabéry Niger" if DISTRICT == "Banibangou"
replace district_country = "Bagoué Côte d'Ivoire" if DISTRICT == "TENGRELA"
replace district_country = "Louga Senegal" if DISTRICT == "Coki"
replace district_country = "Louga Senegal" if DISTRICT == "Dahra"
replace district_country = "Rufisque Senegal" if DISTRICT == "Diamniadio"
replace district_country = "Thiès Senegal" if DISTRICT == "Joal-Fadiouth"
replace district_country = "Thiès Senegal" if DISTRICT == "Poponguine"

sort district_country
save "weekly_inc_clean.dta", replace

//first round of data cleaning, will merge with the original GADM district names in the excel file to see which outbreaks can be assigned to a single district
//this then creates the weekly_inc_easy_merge file
clear
import excel using "Epidemic YN.xlsx", firstrow clear
sort district_country
merge district_country using "weekly_inc_clean.dta"
tab _merge
gen epidemic = 0
replace epidemic = 1 if _merge == 3
drop if _merge == 2
sort district_country2
drop _merge
sort district_country
sort district_country
save "weekly_inc_easymerge.dta", replace

//at this step we filter the data frame, changing some names so we can determine which outbreaks are unable to be matched, these will then be mapped to a seperate WHO polio file to assign outbreaks
// the fiile weekly incidence missing is used here
clear
import excel using "Epidemic YN.xlsx", firstrow clear
sort district_country
merge district_country using "weekly_inc_clean.dta"
tab _merge
gen epidemic = 0
replace epidemic = 1 if _merge == 3
drop if _merge == 1
drop if _merge == 3
drop _merge
sort district_country
replace district_country = "1Er Arrondissement Central African Republic" if district_country == "1er Arrondissement Central African Republic"
replace district_country = "3E Arrondissement Central African Republic" if district_country == "3er Arrondissement Central African Republic"
replace district_country = "Abuzi Democratic Republic of the Congo" if district_country == "ABUZI Democratic Republic of the Congo"
replace district_country = "Abja Democratic Republic of the Congo" if district_country == "ADJA Democratic Republic of the Congo"
replace district_country = "Ariwara Democratic Republic of the Congo" if district_country == "ARIWARA Democratic Republic of the Congo"
replace district_country = "Zone 1 Ethiopia" if district_country == "Abobo woreda Ethiopia"
replace district_country = "Central Tigray Ethiopia" if district_country == "Adwa Town Ethiopia"
drop code
drop COUNTRY_xx
drop NAME_1_xx
drop GID_2x
drop NAME_2_x
drop Epidemic_YN
sort district_country
save "weeklyincidencemissing.dta", replace

//this section is done post Intersection cleaning in step 3
clear
* Import data from Excel file "filtered_intersections30_weekly.xlsx" and save it as Stata data file
import excel "C:\Users\mvc32\OneDrive - University of Cambridge\Documents\Climate_meningitis_belt\Disease_data\filtered_intersections30_weekly.xlsx", sheet("Sheet1") firstrow
sort district_country
save "filtered_intersections30_weekly.dta", replace

clear

* Import data from Excel file "filtered_intersections50_weekly.xlsx" and save it as Stata data file
import excel "C:\Users\mvc32\OneDrive - University of Cambridge\Documents\Climate_meningitis_belt\Disease_data\filtered_intersections50_weekly.xlsx", sheet("Sheet1") firstrow
sort district_country
save "filtered_intersections50_weekly.dta", replace

clear

* Import data from Excel file "Epidemic YN.xlsx" and prepare for merging
import excel using "Epidemic YN.xlsx", firstrow clear
sort district_country

* Merge with the dataset "weekly_inc_easymerge.dta"
merge district_country using "weekly_inc_easymerge.dta"
tab epidemic

* Save the merged dataset with updated epidemic variable
save "weekly_inc_easymerge.dta", replace
sort district_country
use "weekly_inc_easymerge.dta"
drop _merge
sort district_country

* Merge with the dataset "filtered_intersections50_weekly.dta"
merge district_country using "filtered_intersections50_weekly.dta"

* Update epidemic variable based on merge results
drop weekly_epidemics
replace epidemic = 1 if _merge == 3
drop _merge
save "weekly_inc_easymerge.dta", replace

clear

* Use the updated dataset "weekly_inc_easymerge.dta"
use "weekly_inc_easymerge.dta"
sort district_country

* Merge with the dataset "filtered_intersections30_weekly.dta"
merge district_country using "filtered_intersections30_weekly.dta"

* Update epidemic variable based on merge results
replace epidemic = 1 if _merge == 3
drop _merge
save "weekly_inc_totalmerge.dta", replace

* Export the final dataset to Excel, excluding unnecessary variables
export excel using "totalweeklyepidemic", firstrow(variables) replace

* Note: Additional code for dropping unnecessary variables
drop Epidemic_YN
drop A
drop YEAR
drop DISTRICT
drop POP
drop Attack_Rate
drop dup

save "weekly_inc_totalmerge.dta", replace
export excel using "totalweeklyepidemic", firstrow(variables) replace