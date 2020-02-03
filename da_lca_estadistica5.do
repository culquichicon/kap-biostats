local dir `autocd'
cd "`autocd'"
use "BD_Conocimientos_bioestadística_lca.dta",clear


* Cleaning-up
#delimit;
global items ="
c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16
";
#delimit cr;

#delimit;
global covarch ="
epidemio_estudios bioestadistica_estudios mbe_estudios publicacion_cientif 
asesor_tesis extranjero_estudios
";
#delimit cr;

label define correcta 1 "Correcta" 0 "Incorrecta", replace

quietly {
foreach var in $items $covarch{
recode `var' (0=1) (1=0)
lab values `var' correcta
}
}

drop notaporcentual notaporcentual16

gen nota16 = (c1 + c2 + c3 + c4 + c5 + c6 + c7 + c8 +c9 + c10 + c11 + c12 + c13 + c14 + c15 + c16)/16
gen nota12 = (c1 + c2 + c3 + c5 + c6 + c8 +c9 + c11 + c12 + c13 + c14 + c15)/12

#delimit;
global items15 ="
c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c15 c16
";
#delimit cr;



#delimit;
global ss ="
c1 c2 c3 c5 c6 c8 c9 c11 c12 c13 c14 c15
";
#delimit cr;


#delimit;
global covar="
sexo edad essalud_minsa uni_pregrado profesionalizacion interno_residente
ciudad_hospital ciudad_universidad programa_residencia grado_academico 
extranjero_estudios epidemio_estudios bioestadistica_estudios mbe_estudios 
lee_revistas publicacion_cientif asesor_tesis
";
#delimit cr;


label define sexo 0 "masculino" 1 "femenino"
label values sexo sexo

label define edad 0 "21-25" 1 "26-30" 2 "31-35" 3 "36-40" 4 "41-45" 5 ">=46"
label values edad edad

label define años_egresado 0 "<1" 1 "1-3" 2 "4-10" 3 "11-20"
label values years_post_uni años_egresado

label define lee_revistas 0 "No lee" 1 "Lee 01 revista" 2 "Más de 1 revista"
label values lee_revistas lee_revistas

rename lee_otras_rev2 otras_revistas

label define otras_revistas 0 "si" 1 "no"
label values otras_revistas otras_revistas

drop especialidad
drop lee_otras_rev


label def profesionalizaciondef 0 "No aplica" 1"Medico General" 2"Especialista"
label val profesionalizacion profesionalizaciondef 


						*****************************
						*** Latent Class Analysis ***
						*****************************
								
*LCA con 15 preguntas
gsem ($items15 <- ),logit lclass(C 2) showtolerance technique(bfgs)
estat lcprob
estat lcmean
estat lcgof

predict cposta*, classposteriorpr
egen max1 = rowmax(cposta*)
generate predclass1 = 1 if cposta1==max1
replace predclass1 = 2 if cposta2==max1
tabulate predclass1


list in 1/5, abbrev(10) compress


gsem ($items <- ),logit lclass(C 3) nonrtolerance

*/

* LCA con 12 preguntas 
gsem ($ss <- ), logit lclass(C 2) showtolerance technique(bfgs)
estat lcprob
estat lcmean
estat lcgof


margins, 	predict(outcome(c1) class(1)) ///
			predict(outcome(c2) class(1)) ///
			predict(outcome(c3) class(1)) ///
			predict(outcome(c5) class(1)) ///
			predict(outcome(c6) class(1)) ///
			predict(outcome(c8) class(1)) ///
			predict(outcome(c9) class(1)) ///
			predict(outcome(c11) class(1)) ///
			predict(outcome(c12) class(1)) ///
			predict(outcome(c13) class(1)) ///
			predict(outcome(c14) class(1)) ///
			predict(outcome(c15) class(1)) 
marginsplot, recast(bar) title("Class 1") xtitle("") ///
				xlabel(1 "c1" 2 "c2" 3 "c3" 5 "c5" 6 "c6" ///
				8 "c8" 9 "c9" 11 "c11" 12 "c12" 13 "c13" 14 "c14" ///
				15 "c15", angle(45)) ///
				ytitle("Predicted mean") ylabel(0(0.1)1) name(class1)

margins, 	predict(outcome(c1) class(2)) ///
			predict(outcome(c2) class(2)) ///
			predict(outcome(c3) class(2)) ///
			predict(outcome(c5) class(2)) ///
			predict(outcome(c6) class(2)) ///
			predict(outcome(c8) class(2)) ///
			predict(outcome(c9) class(2)) ///
			predict(outcome(c11) class(2)) ///
			predict(outcome(c12) class(2)) ///
			predict(outcome(c13) class(2)) ///
			predict(outcome(c14) class(2)) ///
			predict(outcome(c15) class(2)) 
marginsplot, recast(bar) title("Class 2") xtitle("") ///
				xlabel(1 "c1" 2 "c2" 3 "c3" 5 "c5" 6 "c6" ///
				8 "c8" 9 "c9" 11 "c11" 12 "c12" 13 "c13" 14 "c14" ///
				15 "c15", angle(45)) ///
				ytitle("Predicted mean") ylabel(0(0.1)1) name(class2)


graph combine class1 class2, cols(2)


predict cpostb*, classposteriorpr
sum cpostb*
egen max2 = rowmax(cpostb*)
generate predclass2 = 1 if cpostb1==max2
replace predclass2 = 2 if cpostb2==max2
tabulate predclass2



***Analisis Bivariado***

foreach i in $covar{
	ranksum nota12, by (`i')
}
*edad programa_residencia excluir grado_academico


foreach i in profesionalizacion lee_revistas{
	ranksum nota12 if `i' != 0, by (`i')
} 

tab ciudad_hospita2 predclass2, chi2 col row // me sale esto: exceeded memory limits using exact(1); try again with larger #; see help tabulate for details
tab ciudad_universidad predclass2, exp chi2 exact col row // el mismo problema que con las ciudades de los hospitales.
tab docente_invest predclass2, exp chi2 exact col row // ninguno fue docente, así que esta debería salir; solo hay 1 categoría, no tendría sentido buscar asociación.



**Variables numéricas**
*con años de egresado
bys predclass2: swilk years_post_uni
oneway years_post_uni predclass2, bonferroni tab

kwallis years_post_uni, by(predclass2)
by predclass2, sort : centile years_post_uni, centile(25 50 75)

*con años de residencia
bys predclass2: swilk year_residence
oneway year_residence predclass2
an year_residence predclass2
kwallis year_residence, by(predclass2)
by predclass2, sort : centile year_residence, centile(25 50 75)

*con años desde la última publicación
bys predclass2: swilk year_last_publication
oneway year_last_publication predclass2
an year_last_publication predclass2
kwallis year_last_publication, by(predclass2)
by predclass2, sort : centile year_last_publication, centile(25 50 75)

***ASOCIADOS: ciudad_hospital (aparentemente, ver problema) essalud_minsa ciudad_universidad (aparentemente) programa_residencia grado_academico 
// epidemio_estudios bioestadistica_estudios lee_revistas publicacion_cientif



							*********************
							*** Nested models ***
							*********************
				
*** Internos
quietly {
regress nota12 sexo edad essalud_minsa uni_pregrado extranjero_estudios /// 
					ciudad_hospital essalud_minsa ciudad_universidad ///
					epidemio_estudios bioestadistica_estudios mbe_estudios ///
					lee_revistas publicacion_cientif if interno_residente==0, vce(robust)
}
gen nomiss = e(sample)


	* Level 1
eststo m_0: quietly regress nota12 if nomiss==1

foreach i in sexo edad essalud_minsa uni_pregrado extranjero_estudios /// 
					ciudad_hospital ciudad_universidad ///
					epidemio_estudios bioestadistica_estudios mbe_estudios ///
					lee_revistas publicacion_cientif {
eststo m_`i': quietly regress nota12 ///
					`i' if nomiss==1
lrtest m_0 m_`i'
sca df_`i'=r(df)
sca p_`i'=r(p)
sca chi2_`i'=r(chi2)
}


eststo clear
matrix drop _all
scalar drop _all

/* publicacion_cientif selected */


	* Level 2
eststo m_0: quietly regress nota12 publicacion_cientif if nomiss==1

foreach i in sexo edad essalud_minsa uni_pregrado extranjero_estudios /// 
					ciudad_hospital ciudad_universidad ///
					epidemio_estudios bioestadistica_estudios mbe_estudios ///
					lee_revistas {
eststo m_`i': quietly regress nota12 publicacion_cientif ///
					`i' if nomiss==1
lrtest m_0 m_`i'
sca df_`i'=r(df)
sca p_`i'=r(p)
sca chi2_`i'=r(chi2)
}


eststo clear
matrix drop _all
scalar drop _all

/* ciudad_hospital selected */


	* Level 3
eststo m_0: quietly regress nota12 publicacion_cientif ciudad_hospital if nomiss==1

foreach i in sexo edad essalud_minsa uni_pregrado extranjero_estudios /// 
					ciudad_universidad ///
					epidemio_estudios bioestadistica_estudios mbe_estudios ///
					lee_revistas {
eststo m_`i': quietly regress nota12 publicacion_cientif ciudad_hospital ///
					`i' if nomiss==1
lrtest m_0 m_`i'
sca df_`i'=r(df)
sca p_`i'=r(p)
sca chi2_`i'=r(chi2)
}


eststo clear
matrix drop _all
scalar drop _all

/* extranjero_estudios selected */


	* Level 4
eststo m_0: quietly regress nota12 publicacion_cientif ciudad_hospital ///
					extranjero_estudios if nomiss==1

foreach i in sexo edad essalud_minsa uni_pregrado /// 
					ciudad_universidad ///
					epidemio_estudios bioestadistica_estudios mbe_estudios ///
					lee_revistas {
eststo m_`i': quietly regress nota12 publicacion_cientif ciudad_hospital ///
					extranjero_estudios `i' if nomiss==1
lrtest m_0 m_`i'
sca df_`i'=r(df)
sca p_`i'=r(p)
sca chi2_`i'=r(chi2)
}


eststo clear
matrix drop _all
scalar drop _all

/* No variable selected */


*** Regression models
** Simple linear regressions
regress nota12 ib4.edad if nomiss==1, vce(robust) eform(OR)

regress nota12 ib2.ciudad_universidad if nomiss==1, vce(robust) eform(OR)


foreach i in sexo  essalud_minsa uni_pregrado ciudad_universidad ///
					epidemio_estudios bioestadistica_estudios mbe_estudios ///
					lee_revistas ///
					publicacion_cientif ciudad_hospital extranjero_estudios {
	regress nota12  i.`i' if nomiss==1, vce(robust) eform(OR)					
}


** Parsimonious regression model
regress nota12 publicacion_cientif ciudad_hospital extranjero_estudios ///
				if nomiss==1, vce(robust) eform(OR)

** Adjusted Parsimonious regression model
regress nota12 publicacion_cientif ciudad_hospital extranjero_estudios ///
				ib4.edad if nomiss==1, vce(robust) eform(OR)

regress nota12 publicacion_cientif ciudad_hospital extranjero_estudios ///
				ib2.ciudad_universidad if nomiss==1, vce(robust) eform(OR)

				
foreach i in sexo  essalud_minsa uni_pregrado ciudad_universidad ///
					epidemio_estudios bioestadistica_estudios mbe_estudios ///
					lee_revistas {
	regress nota12 publicacion_cientif ciudad_hospital extranjero_estudios ///
					i.`i' if nomiss==1, vce(robust) eform(OR)					
}
					


*** Residents
quietly {
regress nota12 sexo edad essalud_minsa uni_pregrado profesionalizacion ///
					programa_residencia grado_academico extranjero_estudios /// 
					ciudad_hospital essalud_minsa ciudad_universidad ///
					epidemio_estudios bioestadistica_estudios mbe_estudios ///
					lee_revistas publicacion_cientif if interno_residente==1, vce(robust)
}
gen nomiss2 = e(sample)


	* Level 1
eststo m_0: quietly regress nota12 if nomiss2==1

foreach i in sexo edad essalud_minsa uni_pregrado extranjero_estudios /// 
					ciudad_hospital essalud_minsa ciudad_universidad ///
					epidemio_estudios bioestadistica_estudios mbe_estudios ///
					lee_revistas publicacion_cientif {
eststo m_`i': quietly regress nota12 ///
					`i' if nomiss2==1
lrtest m_0 m_`i'
sca df_`i'=r(df)
sca p_`i'=r(p)
sca chi2_`i'=r(chi2)
}


eststo clear
matrix drop _all
scalar drop _all

/* ciudad_hospital selected */


	* Level 2
eststo m_0: quietly regress nota12 ciudad_hospital if nomiss2==1

foreach i in sexo edad essalud_minsa uni_pregrado extranjero_estudios /// 
					ciudad_universidad ///
					epidemio_estudios bioestadistica_estudios mbe_estudios ///
					lee_revistas publicacion_cientif {
eststo m_`i': quietly regress nota12 ciudad_hospital ///
					`i' if nomiss2==1
lrtest m_0 m_`i'
sca df_`i'=r(df)
sca p_`i'=r(p)
sca chi2_`i'=r(chi2)
}


eststo clear
matrix drop _all
scalar drop _all

/* bioestadistica_estudios selected */


	* Level 3
eststo m_0: quietly regress nota12 ciudad_hospital bioestadistica_estudios if nomiss2==1

foreach i in sexo edad essalud_minsa uni_pregrado extranjero_estudios /// 
					ciudad_universidad ///
					epidemio_estudios  mbe_estudios ///
					lee_revistas publicacion_cientif {
eststo m_`i': quietly regress nota12 ciudad_hospital bioestadistica_estudios ///
					`i' if nomiss2==1
lrtest m_0 m_`i'
sca df_`i'=r(df)
sca p_`i'=r(p)
sca chi2_`i'=r(chi2)
}


eststo clear
matrix drop _all
scalar drop _all

/* edad selected */


	* Level 4
eststo m_0: quietly regress nota12 ciudad_hospital bioestadistica_estudios /// 
					edad if nomiss2==1

foreach i in sexo essalud_minsa uni_pregrado extranjero_estudios /// 
					ciudad_universidad ///
					epidemio_estudios  mbe_estudios ///
					lee_revistas publicacion_cientif {
eststo m_`i': quietly regress nota12 ciudad_hospital bioestadistica_estudios ///
					edad `i' if nomiss2==1
lrtest m_0 m_`i'
sca df_`i'=r(df)
sca p_`i'=r(p)
sca chi2_`i'=r(chi2)
}


eststo clear
matrix drop _all
scalar drop _all

/* essalud_minsa selected */


	* Level 5
eststo m_0: quietly regress nota12 ciudad_hospital bioestadistica_estudios /// 
					edad essalud_minsa if nomiss2==1

foreach i in sexo uni_pregrado extranjero_estudios /// 
					ciudad_universidad ///
					epidemio_estudios  mbe_estudios ///
					lee_revistas publicacion_cientif {
eststo m_`i': quietly regress nota12 ciudad_hospital bioestadistica_estudios ///
					edad essalud_minsa `i' if nomiss2==1
lrtest m_0 m_`i'
sca df_`i'=r(df)
sca p_`i'=r(p)
sca chi2_`i'=r(chi2)
}


eststo clear
matrix drop _all
scalar drop _all

/* epidemio_estudios selected */


	* Level 6
eststo m_0: quietly regress nota12 ciudad_hospital bioestadistica_estudios /// 
					edad essalud_minsa epidemio_estudios if nomiss2==1

foreach i in sexo uni_pregrado extranjero_estudios /// 
					ciudad_universidad ///
					  mbe_estudios ///
					lee_revistas publicacion_cientif {
eststo m_`i': quietly regress nota12 ciudad_hospital bioestadistica_estudios ///
					edad essalud_minsa epidemio_estudios `i' if nomiss2==1
lrtest m_0 m_`i'
sca df_`i'=r(df)
sca p_`i'=r(p)
sca chi2_`i'=r(chi2)
}


eststo clear
matrix drop _all
scalar drop _all

/* no variable selected */


*** Regression models
** Simple linear regression
regress nota12  ib2.ib4.edad if nomiss2==1, eform (OR) vce(robust)
regress nota12  ib2.ciudad_hospital if nomiss2==1, eform (OR) vce(robust)

foreach i in sexo uni_pregrado extranjero_estudios mbe_estudios ///
					lee_revistas publicacion_cientif bioestadistica_estudios ///
					essalud_minsa epidemio_estudios{
	regress nota12  `i' if nomiss2==1, eform (OR) vce(robust)					
}


** Parsimonios regression model
regress nota12 ib2.ciudad_hospital i.bioestadistica_estudios ib4.edad ///
				i.essalud_minsa i.epidemio_estudios if nomiss2==1, eform (OR) vce(robust)


** Adjusted parsimonious regression model
regress nota12 ib2.ciudad_hospital i.bioestadistica_estudios ib4.edad ///
				i.essalud_minsa i.epidemio_estudios ib6.ciudad_universidad if nomiss2==1, eform (OR) vce(robust)

foreach i in sexo uni_pregrado extranjero_estudios mbe_estudios ///
					lee_revistas publicacion_cientif {
	regress nota12 ib2.ciudad_hospital i.bioestadistica_estudios ib4.edad ///
					i.essalud_minsa i.epidemio_estudios if nomiss2==1, eform (OR) vce(robust)					
}


