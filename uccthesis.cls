%
%% This is file `uccthesis.cls',
%% 
%% Copyright 1999-2011 University College Cork, Ireland
%% Peter Flynn, UCC Computer Centre Electronic Publishing Unit <pflynn@ucc.ie>
%%
%% This is v0.91 (May 2011)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING 
%%
%% This is not yet a fully-operational LaTeX2e package. It is
%% currently distributed on test for evaluation as a .cls file
%% with separate documentation. When testing is complete, it
%% will become a .dtx and .ins file in the normal way, except 
%% that it will be restricted to UCC downloads only because it
%% will contain a copy of the UCC crest. The .cls file and the
%% documentation will continue to be available as separate files
%% or the benefit of non-UCC users who may wish to look at it.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% IDENTIFICATION
%
\NeedsTeXFormat{LaTeX2e}[2009/09/24]
\def\fileversion{$\beta$\thinspace0.91}
\ProvidesClass{uccthesis}[2011/05/03 UCC Thesis \fileversion]
\let\uccthesisfileversion\fileversion
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% PRELIMINARY DECLARATIONS to enable options to work
%
\RequirePackage{ifthen}
%
% For each divisional layer (School, Department, Institute, Hospital,
% Centre, Unit [deprecated]), create three control sequences:
%
%  1. a "handle" or prefix/suffix;
%  2. a default value of \relax for the storage of the value;
%  3. a command to allow the assignment of a value. 
%
% The command takes one optional argument and one compulsory:
% the compulsory argument is the value to assign; if there is no
% optional argument, then the value is assigned with the handle
% prefixed or suffixed. If the optional argument brackets are
% present but with no value supplied, the compulsory value is
% assigned *without* the handle; if a value is supplied, then 
% the value may be used in conjunction with the value of the
% compulsory argument. Examples:
%
% \centre{Computer} produces Computer Centre
%
% \centre[]{Centre for Excellence in Design}
% produces
% Centre for Excellence in Design
%
% \centre[Alimentary Pharmabiotic Centre]{APC}
% produces 
% Alimentary Pharmabiotic Centre (APC)
%
% This applies to Institutes, Hospitals, Centres, and Units only. For
% Schools and Departments, the third format currently ignores the
% specified optional argument.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING 
%
% The UCC organisational model is currently broken. Colleges have
% not replaced Faculties, as was envisaged, but added to them; and
% Schools have likewise not replaced Departments. There is no longer a
% strict hierarchy, as many Schools and Departments belong to more
% than one College or Faculty, so there is no clear identity.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
\def\@schoolhandle{School of}
\def\@school{\relax}
\newcommand{\school}[2][\relax]{%
% if the optional argument is present but null, 
% accept the main argument as gospel and make no changes
  \ifthenelse{\equal{#1}{}}{\gdef\@school{#2}}{%
    \if#1\relax
% if the optional argument is not present,
% prepend the handle
      \gdef\@school{\@schoolhandle\ #2}%
% otherwise treat it as a replacement handle the handle
    \else\gdef\@school{#1 #2}\fi}}
%
% same for depts
\def\@departmenthandle{Department of}
\def\@department{\relax}
\newcommand{\department}[2][\relax]{%
  \ifthenelse{\equal{#1}{}}{\gdef\@department{#2}}{%
    \if#1\relax
      \gdef\@department{\@departmenthandle\ #2}%
    \else\gdef\@department{#1 #2}\fi}}
%
% for institutes, use non-null optional argument as acronym
\def\@institutehandle{Institute}
\def\@institute{\relax}
\newcommand{\institute}[2][\relax]{%
  \ifthenelse{\equal{#1}{}}{\gdef\@institute{#2}}{%
    \if#1\relax
      \gdef\@institute{#2 \@institutehandle}%
    \else\gdef\@institute{#2 (#1)}\fi}}
%
% ditto for hospitals
\def\@hospitalhandle{Hospital}
\def\@hospital{\relax}
\newcommand{\hospital}[2][\relax]{%
  \ifthenelse{\equal{#1}{}}{\gdef\@hospital{#2}}{%
    \if#1\relax
      \gdef\@hospital{#2 \@hospitalhandle}%
    \else\gdef\@hospital{#2 (#1)}\fi}}
%
% and centres
\def\@centrehandle{Centre}
\def\@centre{\relax}
\newcommand{\centre}[2][\relax]{%
  \ifthenelse{\equal{#1}{}}{\gdef\@centre{#2}}{%
    \if#1\relax
      \gdef\@centre{#2 \@centrehandle}%
    \else\gdef\@centre{#2 (#1)}\fi}}
%
% and units
\def\@unithandle{Unit}
\def\@unit{\relax}
\newcommand{\unit}[2][\relax]{%
  \ifthenelse{\equal{#1}{}}{\gdef\@unit{#2}}{%
    \if#1\relax
      \gdef\@unit{#2 \@unithandle}%
    \else\gdef\@unit{#2 (#1)}\fi}}
%
% Define a string for the titling of the Acknowledgements section, so
% that it can be redefined in the gaeilge option.
%
\newcommand\ackname{Acknowledgements}
%
% Define a command to record the bibliographic reference format that
% will be required for each College/Faculty/School/Department option.
% This holds the name of the .bst file appropriate for each discipline
% and [optionally] the package name needed to implement it. A possible
% third argument may be introduced later to hold the command required 
% for a full citation in a block quotation.
%
\newcommand{\@usebib}[4][\relax]{%
  \if#1\relax\else
    \AtEndOfPackage{\RequirePackage{#1}}\fi
  \AtEndDocument{\bibliographystyle{#2}}%
}
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% OPTIONS
%
% Draft mode is the default, but allow it to be passed to report
\def\class@mode{draft}
%
\DeclareOption{draft}{\setlength\overfullrule{5pt}%
  \def\class@mode{draft}%
  \AtEndOfPackage{%
    \lfoot{\scriptsize\ttfamily Draft: \jobname}%
    \rfoot{\scriptsize\ttfamily \TODAY T\now:00}}%
  \typeout{DRAFT MODE}}
%
\DeclareOption{final}{\setlength\overfullrule{0pt}%
  \def\class@mode{final}%
  \sloppy\raggedbottom
  \typeout{Final mode}}
%
\def\class@size{11pt}
\DeclareOption{10pt}{\def\class@size{10pt}}
\DeclareOption{11pt}{\def\class@size{11pt}}
\DeclareOption{12pt}{\def\class@size{12pt}}
%
\def\class@paper{a4paper}
\DeclareOption{a4paper}{\def\class@size{a4paper}}
\DeclareOption{letterpaper}{\def\class@size{letterpaper}}
%
% Provide for theses in Irish
%
\DeclareOption{gaeilge}{%
  \RequirePackage[english,irish]{babel}%
  \RequirePackage{eiad}%
  \AtEndOfPackage{%
    \renewcommand\ackname{Bu\'\i ochas}
    \institution{\so{COL{\'A}ISTE NAISI{\'U}NTA 
	NA h{\'E}IREANN, CORCAIGH}}
    \def\today{\number\day\space\ifcase\month\or
      Ean\'a\i r\or Feabhra\or M\'arta\or A\i bre\'an\or 
      Bealta\i ne\or Me\i theamh\or U\'u\i l\or L\'unasa\or 
      Me\'an F\'omha\i r\or De\i readh F\'omha\i r\or 
      Samha\i n\or M\'\i\ na Nollag\fi\space
      \number\year}%
    \def\@schoolhandle{Scoil an}
    \def\@departmenthandle{Roinn an}
    \def\@institutehandle{Institúid }
    \def\@hospitalhandle{Spidéal }
    \def\@centrehandle{???}
    \def\@unithandle{???}
    \raggedright
}}
% CJK needs a preloaded package, so flag it with an option
\newif\ifcjk\cjkfalse
\DeclareOption{cjk}{\cjktrue}
%
% Schools and Departments
%
% This list can be extracted and parsed into the tables for the
% documentation with the command in the getopts script:
% grep -nB 4 '\ \\@usebib[\[{]' `kpsewhich uccthesis.cls` |\
%    grep -v '%%' | sort | uniq | awk -f getopts.awk
%
%%
%%Arts
\DeclareOption{geog}{%
  \department{Geography}% acsss
  \@usebib[harvard]{kluwer}{Harvard}{}}%
\DeclareOption{arch}{%
  \department{Archaeology}% acsss
  \@usebib[harvard]{kluwer}{Harvard}{}}%
\DeclareOption{planning}{%
  \school{Geography and Archaeology: The Human Environment}% acsss
  \@usebib[harvard]{kluwer}{Harvard}{}}%
%%
\DeclareOption{apsych}{%
  \school{Applied Psychology}% acsss
  \@usebib[apacite]{apacite}{APA}{}}% pflynn@ucc.ie
\DeclareOption{apsoc}{%
  \school{Applied Social Studies}% acsss
  \@usebib[harvard]{kluwer}{Harvard}{}}%
\DeclareOption{asian}{% preloads the \textsf{CJK} package (Chinese/Japanese/Korean)
  \school{Asian Studies}% acsss
  \@usebib[harvard]{kluwer}{Harvard}{}%
  \cjktrue}
\DeclareOption{educ}{%
  \school{Education}% acsss
  \@usebib[harvard]{kluwer}{Harvard}{}}%
\DeclareOption{eng}{%
  \school{English}% acsss
  \@usebib[harvard]{kluwer}{Harvard}{}}%
\DeclareOption{hist}{%
  \school{History}% acsss
  \@usebib[harvard]{kluwer}{Harvard}{}}%
\DeclareOption{arthist}{%
  \school{History}% acsss
  \department{History of Art}%
  \@usebib[oxford]{oxford_en}{Oxford}{}}% F.Boggi@ucc.ie
\DeclareOption{folketh}{%
  \department{Folklore and Ethnology}% acsss
  \@usebib[harvard]{kluwer}{Harvard}{}}%
%%
\DeclareOption{llc}{%
  \school{Languages, Literatures and Cultures}% acsss
  \@usebib[harvard]{kluwer}{Harvard}{}}%
\DeclareOption{french}{%
  \school{Languages, Literatures and Cultures}% acsss
  \department{French}%
  \@usebib[harvard]{kluwer}{Harvard}{}}% default
\DeclareOption{german}{%
  \school{Languages, Literatures and Cultures}% acsss
  \department{German}%
  \@usebib{mla-good}{MLA}{}}% G.Hofmann@ucc.ie
\DeclareOption{hisp}{%
  \school{Languages, Literatures and Cultures}% acsss
  \department{Hispanic Studies}%
  \@usebib[harvard]{kluwer}{Harvard}{}}% default
\DeclareOption{italian}{%
  \school{Languages, Literatures and Cultures}% acsss
  \department{Italian}%
  \@usebib[harvard]{kluwer}{Harvard}{}}% default
\DeclareOption{classics}{%
  \department{Classics}% acsss
  \@usebib[oxford]{oxford_en}{Oxford}{}}% D.Woods@ucc.ie
\DeclareOption{lnag}{% may require the \texttt{\bfseries gaeilge} option
  \school{Scoil Léann na Gaeilge~/ School of Irish Learning}% acsss
  \@usebib[harvard]{kluwer}{Harvard}{}}%
\DeclareOption{emi}{%
  \department{Early and Medieval Irish}% acsss
  \@usebib[harvard]{kluwer}{Harvard}{}}% default
\DeclareOption{smg}{%
  \department{Sean- agus Meán-Ghaeilge}% acsss
  \@usebib[harvard]{kluwer}{Harvard}{}}% default
\DeclareOption{modirish}{%
  \department{Modern Irish}% acsss
  \@usebib[harvard]{kluwer}{Harvard}{}}% default
\DeclareOption{nuag}{%
  \department{Nua-Ghaeilge}% acsss
  \@usebib[harvard]{kluwer}{Harvard}{}}% default
\DeclareOption{bealoideas}{%
  \department[]{Béaloideas~/ Folklore}% acsss
  \@usebib[harvard]{kluwer}{Harvard}{}}% default
%%
\DeclareOption{music}{%
  \school{Music and Theatre}% acsss
  \department{Music}
  \@usebib[harvard]{kluwer}{Harvard}{}}%
\DeclareOption{theatre}{%
  \school{Music and Theatre}% acsss
  \department{Drama and Theatre Studies}%
  \@usebib[harvard]{kluwer}{Harvard}{}}%
%%
\DeclareOption{sociophil}{%
  \school{Sociology and Philosophy}% acsss
  \@usebib[harvard]{dcu}{Harvard}{}}% a.szakolczai@ucc.ie
\DeclareOption{socio}{%
  \department{Sciology}% acsss
  \@usebib[harvard]{kluwer}{Harvard}{}}% default
\DeclareOption{phil}{%
  \department{Philosophy}% acsss
  \@usebib[harvard]{kluwer}{Harvard}{}}% default
%%
\DeclareOption{relig}{%
  \department{Study of Religions}% acsss
  \@usebib[harvard]{kluwer}{Harvard}{}}% default
\DeclareOption{politics}{%
  \department{Politics}% acsss
  \@usebib[harvard]{kluwer}{Harvard}{}}% default
%% Business & Law
\DeclareOption{law}{%
  \school[]{Faculty of Law}% buslaw
  \@usebib[jurabib]{jurabib}{Legal}{}}% pflynn@ucc.ie
%%
\DeclareOption{commerce}{%
  \school[]{Faculty of Commerce}% buslaw
  \@usebib[harvard]{kluwer}{Harvard}{}}%
\DeclareOption{afis}{%
  \school[]{Faculty of Commerce}% buslaw
  \department{Accounting and Finance}%
  \@usebib[harvard]{apsr}{Harvard}{}}% 
\DeclareOption{bis}{%
  \school[]{Faculty of Commerce}% buslaw
  \department{Business Information Systems}%
  \@usebib[harvard]{apsr}{Harvard}{}}% 
\DeclareOption{coop}{%
  \school[]{Faculty of Commerce}% buslaw
  \centre[]{Centre for Co-operative Studies}%
  \@usebib[harvard]{apsr}{Harvard}{}}% 
\DeclareOption{cps}{%
  \school[]{Faculty of Commerce}% buslaw
  \centre[]{Centre for Policy Studies}%
  \@usebib[harvard]{apsr}{Harvard}{}}% 
\DeclareOption{econ}{%
  \school[]{Faculty of Commerce}% buslaw
  \department{Economics}%
  \@usebib[harvard]{apsr}{Harvard}{}}% niall.osullivan@ucc.ie
\DeclareOption{govt}{%
  \school[]{Faculty of Commerce}% buslaw
  \department{Government}%
  \@usebib[harvard]{apsr}{Harvard}{}}% 
\DeclareOption{mgt}{%
  \school[]{Faculty of Commerce}% buslaw
  \department{Management and Marketing}%
  \@usebib[harvard]{apsr}{Harvard}{}}% 
\DeclareOption{mktg}{% is an alternative for \texttt{mgt}
  \school[]{Faculty of Commerce}% buslaw
  \department{Management and Marketing}%
  \@usebib[harvard]{apsr}{Harvard}{}}% 
\DeclareOption{stats}{%
  \department{Statistics}% buslaw/sefs
  \@usebib[harvard]{apsr}{Harvard}{}}% 
%% Medicine & Health
\DeclareOption{cuh}{%
  \hospital{Cork University}% med
  \@usebib{vancouver}{Vancouver}{}}% 
\DeclareOption{mercy}{%
  \hospital{Mercy University}% med
  \@usebib{vancouver}{Vancouver}{}}% 
\DeclareOption{med}{%
  \school{Medicine}% med
  \@usebib{vancouver}{Vancouver}{}}% siun.oflynn@ucc.ie
\DeclareOption{anat}{%
  \department{Anatomy}% med
  \@usebib{vancouver}{Vancouver}{}}% siun.oflynn@ucc.ie
\DeclareOption{biochem}{%
  \department{Biochemistry}% med
  \@usebib{vancouver}{Vancouver}{}}% siun.oflynn@ucc.ie
\DeclareOption{epipub}{%
  \school{Medicine}% med
  \department{Epidemiology and Public Health}%
  \@usebib{vancouver}{Vancouver}{}}% i.perry@ucc.ie
\DeclareOption{gp}{%
  \school{Medicine}% med
  \department{General Practice}%
  \@usebib{vancouver}{Vancouver}{}}% siun.oflynn@ucc.ie
\DeclareOption{medicine}{%
  \school{Medicine}% med
  \department{Medicine}%
  \@usebib{vancouver}{Vancouver}{}}% siun.oflynn@ucc.ie
\DeclareOption{obsgyn}{%
  \school{Medicine}% med
  \department{Obstetrics and Gynaecology}%
  \@usebib{vancouver}{Vancouver}{}}% siun.oflynn@ucc.ie
\DeclareOption{pch}{%
  \school{Medicine}% med
  \department{Paediatrics and Child Health}%
  \@usebib{vancouver}{Vancouver}{}}% siun.oflynn@ucc.ie
\DeclareOption{path}{%
  \school{Medicine}% med
  \department{Pathology}%
  \@usebib{vancouver}{Vancouver}{}}% siun.oflynn@ucc.ie
\DeclareOption{pharmthera}{%
  \school{Medicine}% med
  \department{Pharmacology and Therapeutics}%
  \@usebib{vancouver}{Vancouver}{}}% siun.oflynn@ucc.ie
\DeclareOption{physio}{%
  \department{Physiology}% med/sefs
  \@usebib{vancouver}{Vancouver}{}}% siun.oflynn@ucc.ie
\DeclareOption{psychiatry}{%
  \school{Medicine}% med
  \department{Psychiatry}%
  \@usebib{vancouver}{Vancouver}{}}% T.Dinan@ucc.ie
\DeclareOption{surg}{%
  \school{Medicine}% med
  \department{Surgery}%
  \@usebib{vancouver}{Vancouver}{}}% siun.oflynn@ucc.ie
%%
\DeclareOption{nursing}{%
  \school{Nursing and Midwifery}% med
  \@usebib[harvard]{dcu}{Harvard}{}}% J.Hegarty@ucc.ie
\DeclareOption{midwifery}{%
  \school{Nursing and Midwifery}% med
  \@usebib[harvard]{dcu}{Harvard}{}}% J.Hegarty@ucc.ie
%%
\DeclareOption{pharm}{%
  \school{Pharmacy}% med
  \@usebib[harvard]{kluwer}{Harvard}{}}%
%%
\DeclareOption{clinther}{%
  \school{Clinical Therapies}% med
  \@usebib[harvard]{kluwer}{Harvard}{}}%
\DeclareOption{occther}{%
  \school{Clinical Therapies}% med
  \department{Occupational Therapy}%
  \@usebib[harvard]{kluwer}{Harvard}{}}%
\DeclareOption{speech}{%
  \school{Clinical Therapies}% med
  \department{Speech and Hearing Sciences}%
  \@usebib[apacite]{apacite}{APA}{}}% F.Gibbon@ucc.ie
\DeclareOption{hearing}{%
  \school{Clinical Therapies}% med
  \department{Speech and Hearing Sciences}%
  \@usebib[apacite]{apacite}{APA}{}}% F.Gibbon@ucc.ie
%%
\DeclareOption{life}{%
  \school{Life Sciences}% med
  \@usebib[harvard]{agsm}{Harvard}{}}%
%%
\DeclareOption{radio}{%
  \school{Medicine}% med
  \department{Radiology}%
  \@usebib[harvard]{agsm}{Harvard}{}}%
%%
\DeclareOption{denthosp}{%
  \hospital[]{University Dental School and Hospital}% med
  \@usebib{vancouver}{Vancouver}{}}% f.allen@ucc.ie 
\DeclareOption{dentsurg}{%
  \hospital[]{University Dental School and Hospital}% med
  \department{Dental Surgery}
  \@usebib{vancouver}{Vancouver}{}}% f.allen@ucc.ie
\DeclareOption{dentrest}{%
  \hospital[]{University Dental School and Hospital}% med
  \school{Dental Hygiene}
  \department{Restorative Dentistry}%
  \@usebib{vancouver}{Vancouver}{}}% f.allen@ucc.ie
\DeclareOption{oralhealth}{%
  \hospital[]{University Dental School and Hospital}% med
  \department{Oral Health and Development}%
  \@usebib{vancouver}{Vancouver}{}}% f.allen@ucc.ie
%% SEFS
\DeclareOption{scifac}{%
  \school[]{Faculty of Science}% sefs
  \@usebib{alpha}{Default}{}}%
%%
\DeclareOption{chem}{%
  \school[]{Faculty of Science}%
  \department{Chemistry}% sefs
  \@usebib[natbib]{achemso}{ACS}{}}% j.sodeau@ucc.ie
\DeclareOption{cs}{%
  \school[]{Faculty of Science}%
  \department{Computer Science}% sefs
  \@usebib{alpha}{Default}{}}%
\DeclareOption{geol}{%
  \school[]{Faculty of Science}% sefs
  \department{Geology}%
  \@usebib{alpha}{Default}{}}%
\DeclareOption{microbio}{%
  \department{Microbiology}% sefs
  \@usebib{alpha}{Default}{}}%
\DeclareOption{physics}{%
  \school[]{Faculty of Science}% sefs
  \department{Physics}%
  \@usebib[natbib]{aipnum4-1}{AIP}{}}% J.McInerney@ucc.ie
%%
\DeclareOption{maths}{%
  \school{Mathematical Sciences}%
  \department{Mathematics}% sefs
  \@usebib{alpha}{Default}{}}%
\DeclareOption{appmaths}{%
  \department{Applied Mathematics}% sefs
  \@usebib{alpha}{Default}{}}%
%%
\DeclareOption{zeps}{%
  \school{Zoology, Ecology and Plant Science}% sefs
  \@usebib[oxford]{oxford_en}{Oxford}{}}% j.ohalloran@ucc.ie
\DeclareOption{bees}{%
  \school{Biological, Earth and Environmental Sciences}% sefs
  \@usebib[oxford]{oxford_en}{Oxford}{}}% j.ohalloran@ucc.ie
%%
\DeclareOption{engineering}{%
  \school{Engineering}% sefs
  \@usebib[harvard]{agsm}{Harvard}{}}%
\DeclareOption{civil}{%
  \school{Engineering}% sefs
  \department{Civil and Environmental Engineering}%
  \@usebib[harvard]{agsm}{Harvard}{}}% Caitriona de Paor
\DeclareOption{elec}{%
  \school{Engineering}% sefs
  \department{Electrical and Electronic Engineering}%
  \@usebib[harvard]{agsm}{Harvard}{}}% 
\DeclareOption{microelec}{%
  \school{Engineering}% sefs
  \department{Microelectronic Engineering}%
  \@usebib[harvard]{agsm}{Harvard}{}}% 
\DeclareOption{chemeng}{%
  \department{Process and Chemical Engineering}% sefs
  \@usebib[harvard]{agsm}{Harvard}{}}% 
%%
\DeclareOption{food}{%
  \school{Food and Nutritional Sciences}% sefs
  \@usebib{alpha}{Default}{}}%
\DeclareOption{foodbus}{%
  \school{Food and Nutritional Sciences}%
  \department{Food Business and Development}% sefs/buslaw
  \@usebib[harvard]{agsm}{Harvard}{}}%
%%
\DeclareOption{tyndall}{%
  \institute{Tyndall National}% inst
  \@usebib{alpha}{Default}{}}%
\DeclareOption{imi}{%
  \institute{Irish Management}% inst
  \@usebib[harvard]{dcu}{Harvard}{}}%
%
% Metadata
%
\newif\ifthesis
\def\@degree{Something}
\newcommand{\degree}[1]{\gdef\@degree{#1}\thesistrue}
\DeclareOption{phd}{% acsss/buslaw/sefs/med
  \degree{Doctor of Philosophy}}%
\DeclareOption{phdbis}{% buslaw
  \degree{Doctor of Philosophy in Business Information Systems}}%
\DeclareOption{phdcb}{% med
  \degree{Doctor of Philosophy in Cancer Biology}}%
\DeclareOption{phded}{% acsss
  \degree{Doctor of Philosophy in Education~--- Structured}}%
\DeclareOption{phdhsr}{% med
  \degree{Doctor of Philosophy in Health Services Research}}%
\DeclareOption{phdss}{% acsss
  \degree{Doctor of Philosophy in Social Sciences}}%
\DeclareOption{phdtcc}{% acsss
  \degree{Doctor of Philosophy in Texts, Contexts and Cultures}}%
\DeclareOption{md}{% med
  \degree{Doctor of Medicine}}%
\DeclareOption{dmed}{% med
  \degree{Doctor of Medicine}}%
\DeclareOption{dlitt}{% acsss
  \degree{Doctor of Literature}}%
\DeclareOption{dlittcelt}{% acsss
  \degree{Doctor of Celtic Literature}}%
\DeclareOption{deconsc}{% buslaw
  \degree{Doctor of Economic Science}}%
\DeclareOption{dsc}{% sefs
  \degree{Doctor of Science}}%
\DeclareOption{dmus}{% acsss
  \degree{Doctor of Music}}%
\DeclareOption{lld}{% buslaw
  \degree{Doctor of Law}}%
\DeclareOption{dbe}{% buslaw
  \degree{Doctor of Business Economics}}%
\DeclareOption{dclindent}{% med
  \degree{Doctor of Clinical Dentistry}}%
\DeclareOption{dnp}{% med
  \degree{Doctor of Nursing Practice}}%
\DeclareOption{docct}{% med
  \degree{Doctor of Occupational Therapy}}%
\DeclareOption{dsocsci}{% acsss
  \degree{Doctor of Social Science}}%
%%
\DeclareOption{ma}{% acsss
  \degree{Master of Arts}}%
\DeclareOption{mphil}{% acsss
  \degree{Master of Philosophy}}%
\DeclareOption{med}{% acsss
  \degree{Master of Education}}%
\DeclareOption{msocsc}{% acsss
  \degree{Master of Social Science}}%
\DeclareOption{msw}{% acsss
  \degree{Master of Social Work}}%
\DeclareOption{mba}{% buslaw
  \degree{Master of Business Administration}}%
\DeclareOption{mbs}{% buslaw
  \degree{Master of Business Systems}}%
\DeclareOption{mbsbe}{% buslaw
  \degree{Master of Business Systems (Business Economics)}}%
\DeclareOption{mbsbis}{% buslaw
  \degree{Master of Business Systems (Business Information Systems)}}%
\DeclareOption{mbscse}{% buslaw
  \degree{Master of Business Systems (Co-operative and Social Enterprise)}}%
\DeclareOption{mbsbp}{% buslaw
  \degree{Master of Business Systems (Economics of Business Practice)}}%
\DeclareOption{mbseb}{% buslaw
  \degree{Master of Business Systems (Electronic Business)}}%
\DeclareOption{mbsfm}{% buslaw
  \degree{Master of Business Systems (Food Marketing)}}%
\DeclareOption{mbsg}{% buslaw
  \degree{Master of Business Systems (Government)}}%
\DeclareOption{mbshsm}{% buslaw
  \degree{Master of Business Systems (Health Services Management)}}%
\DeclareOption{mbsisbp}{% buslaw
  \degree{Master of Business Systems (Information Systems for Business Performance)}}%
\DeclareOption{mbsieb}{% buslaw
  \degree{Master of Business Systems (Innovation in European Business)}}%
\DeclareOption{mbsippd}{% buslaw
  \degree{Master of Business Systems (International Public Policy and Diplomacy)}}%
\DeclareOption{mbsmimas}{% buslaw
  \degree{Master of Business Systems (Management Information and Managerial Accounting Systems)}}%
\DeclareOption{mbsmm}{% buslaw
  \degree{Master of Business Systems (Management and Marketing)}}%
\DeclareOption{mbsasian}{% buslaw/acsss
  \degree{Master of Business Systems (Asian Business)}}%
\DeclareOption{mcomm}{% buslaw
  \degree{Master of Commerce}}%
\DeclareOption{mcommbe}{% buslaw
  \degree{Master of Commerce (Business Economics)}}%
\DeclareOption{mcommgpp}{% buslaw
  \degree{Master of Commerce (Government and Public Policy)}}%
\DeclareOption{mcommscm}{% buslaw
  \degree{Master of Commerce (Supply Chain Management)}}%
\DeclareOption{mcommbb}{% buslaw
  \degree{Master of Commerce (Supply Chain Management~--- Lean SCM Black Belt)}}%
\DeclareOption{meconsc}{% buslaw
  \degree{Master of Economic Science}}%
\DeclareOption{msccf}{% buslaw
  \degree{Master of Science (Corporate Finance)}}%
\DeclareOption{msche}{% buslaw
  \degree{Master of Science (Health Economics)}}%
\DeclareOption{mscfe}{% buslaw
  \degree{Master of Science (Financial Economics)}}%
%%
\DeclareOption{llm}{% buslaw
  \degree{Master of Law}}%
\DeclareOption{llme}{% buslaw
  \degree{Master of Law (e-Law)}}%
\DeclareOption{llmcj}{% buslaw
  \degree{Master of Law (Criminal Justice)}}%
\DeclareOption{llmp}{% buslaw
  \degree{Master of Law (Practitioner)}}%
%%
\DeclareOption{mds}{% med
  \degree{Master of Dental Science}}%
\DeclareOption{mdph}{% med
  \degree{Master in Dental Public Health}}%
\DeclareOption{mao}{% med
  \degree{Master of Obstetrics}}%
\DeclareOption{mch}{% med
  \degree{Master of Surgery}}%
\DeclareOption{mmedsc}{% med
  \degree{Master in Sport and Exercise Medicine}}%
\DeclareOption{mscog}{% med
  \degree{Master of Science (Obstetrics and Gynaecology)}}%
\DeclareOption{mscoh}{% med
  \degree{Master of Science (Occupational Health)}}%
\DeclareOption{mph}{% med
  \degree{Master in Public Health}}%
\DeclareOption{mscns}{% med
  \degree{Master of Science (Nursing Studies)}}%
\DeclareOption{mscm}{% med
  \degree{Master of Science (Midwifery)}}%
\DeclareOption{msccp}{% med
  \degree{Master of Science (Clinical Pharmacy)}}%
\DeclareOption{mscptqs}{% med
  \degree{Master of Science (Pharmaceutical Technology and Quality Systems)}}%
\DeclareOption{mscebtp}{% med
  \degree{Master of Science (Evidence-Based Therapy Practice)}}%
%%
\DeclareOption{msc}{% sefs
  \degree{Master of Science}}%
\DeclareOption{mscapc}{% sefs
  \degree{Master of Applied Science (Analysis of Pharmaceutical Compounds)}}%
\DeclareOption{mscac}{% sefs
  \degree{Master of Applied Science (Analytical Chemistry)}}%
\DeclareOption{mscbsb}{% sefs
  \degree{Master of Applied Science (Bioinformatics with Systems Biology)}}%
\DeclareOption{mscb}{% sefs
  \degree{Master of Applied Science (Biotechnology)}}%
\DeclareOption{mscea}{% sefs
  \degree{Master of Applied Science (Ecological Assessment)}}%
\DeclareOption{msceac}{% sefs
  \degree{Master of Applied Science (Environmental Analytical Chemistry)}}%
\DeclareOption{mscgis}{% sefs
  \degree{Master of Applied Science (Geographical Information Systems and Remote Sensing)}}%
\DeclareOption{mscmb}{% sefs
  \degree{Master of Applied Science (Marine Biology)}}%
\DeclareOption{mscmmsc}{% sefs
  \degree{Master of Applied Science (Mathematical Modelling and Scientific Computing)}}%
\DeclareOption{mscssmn}{% sefs
  \degree{Master of Applied Science (Software and Systems for Mobile Networks)}}%
\DeclareOption{me}{% sefs
  \degree{Master of Engineering}}%
\DeclareOption{mengsc}{% sefs
  \degree{Master of Engineering Science}}%
\DeclareOption{mengscit}{% sefs
  \degree{Master of Engineering Science (Information Technology in Architecture, Engineering and Construction)}}%
\DeclareOption{mengscme}{% sefs
  \degree{Master of Engineering Science in Mechanical Engineering (Manufacturing, Process and Automation Systems)}}%
\DeclareOption{mengscmd}{% sefs
  \degree{Master of Engineering Science (Microelectronic Design)}}%
\DeclareOption{mengscm}{% sefs
  \degree{Master of Engineering Science (Microelectronics)}}%
\DeclareOption{mengscpbe}{% sefs
  \degree{Master of Engineering Science (Pharmaceutical and Biopharmaceutical Engineering)}}%
\DeclareOption{mengscse}{% sefs
  \degree{Master of Engineering Science (Sustainable Energy)}}%
\DeclareOption{mplan}{% sefs/acsss
  \degree{Master of Planning and Sustainable Development}}%
\DeclareOption{msccoop}{% sefs
  \degree{Master of Science (Co-operative Organisation/Food Marketing/Rural Development)}}%
\DeclareOption{mscfb}{% sefs
  \degree{Master of Science (Food Business)}}%
\DeclareOption{mscfs}{% sefs
  \degree{Master of Applied Science (Food Science)}}%
\DeclareOption{mscfm}{% sefs
  \degree{Master of Applied Science (Food Microbiology)}}%
\DeclareOption{mscns}{% sefs
  \degree{Master of Applied Science (Nutritional Sciences)}}%
\DeclareOption{mscrd}{% sefs
  \degree{Master of Science (Rural Development)}}%
\DeclareOption{msctm}{% sefs
  \degree{Master of Science (Technology Management)}}%
%
\def\@degreehandle{Degree of \@degree}
% Allow it to be used for its own documentation :-)
\DeclareOption{nothesis}{%
  \AtEndOfPackage{%
    \degree{}%
    \supervisor{}%
    \def\@degree{\relax}%
    \thesisfalse
    \newcommand{\env}[1]{\texttt{#1}}
    \newcommand{\package}[1]{\textsf{#1}}
    \renewcommand{\abstractname}{Summary}
    \RequirePackage{mflogo}\let\mf\MF
    \RequirePackage{varioref}%
    \renewcommand{\reftextcurrent}{elsewhere on this page}
    \def\reftextafter{on the \reftextvario{next}{following} page}%
    \labelformat{chapter}{Chapter~#1}
    \labelformat{chapter}{\@chapapp~#1}
    \labelformat{section}{section~#1}
    \labelformat{subsection}{section~#1}
    \labelformat{subsubsection}{section~#1}
    \labelformat{figure}{Figure~#1}
    \labelformat{table}{Table~#1}
    \labelformat{item}{item~#1}
    \newcommand{\BibTeX}{\textsc{Bib}\kern-.08em \TeX}
    \let\BIBTeX\BibTeX
    \let\BiBTeX\BibTeX
    \newcommand{\ConTeXt}{C\kern-.0333emon\-\kern-.0667em\TeX\kern-.0333emt}%
    \def\tubreflect#1{%
      \@ifundefined{reflectbox}{%
        \TBerror{A graphics package must be loaded for \string\XeTeX}%
      }{%
        \ifdim \fontdimen1\font>0pt
        \raise 1.75ex \hbox{\kern.1em\rotatebox{180}{#1}}\kern-.1em
        \else
        \reflectbox{#1}%
        \fi
      }%
    }
    \def\tubhideheight#1{\setbox0=\hbox{#1}\ht0=0pt \dp0=0pt \box0 }
    \DeclareRobustCommand\Xe[1]{\leavevmode
      \tubhideheight{\hbox{X%
          \setbox0=\hbox{\TeX}\setbox1=\hbox{E}%
          \lower\dp0\hbox{\raise\dp1\hbox{\kern-.125em\tubreflect{E}}}%
          \kern-.1667em #1}}}
    \def\XeTeX{\Xe\TeX}
    \def\XeLaTeX{\Xe{\,\LaTeX}}
    \newcommand{\LyX}{L\kern-.1667em\lower.25em\hbox{Y}\kern-.125emX}%
  }%
    \typeout{Not a degree}}
% term papers - experimental
\newif\iftermpaper
\DeclareOption{termpaper}{%
  \AtEndOfPackage{%
    \degree{}%
    \def\@degree{\relax}%
    \degreefalse}%
  \typeout{Term paper}}
%
% Wordprocessor option
%
\DeclareOption{wp}{%
  \AtBeginDocument{%
    \everypar{\vrule depth.2ex width0pt}%
    \RequirePackage{pslatex,ragged2e}%
    \RaggedRight
    \hyphenpenalty=99999 \exhyphenpenalty=99999
    \def\@makechapterhead#1{%
      \begin{center}\normalfont
	\ifnum \c@secnumdepth >\m@ne
	\if@mainmatter
	\LARGE{\sffamily{\@chapapp}\space 
	  \LARGE\thechapter}%
	\par\nobreak
	\vspace*{2\baselineskip}%
	\fi
	\fi
	\interlinepenalty\@M
	\Huge\sffamily #1\par\nobreak
	\vspace*{3\baselineskip}%
    \end{center}}%
    \renewcommand\section{\@startsection {section}{1}{\z@}%
      {-2\baselineskip}%
      {\baselineskip}%
      {\normalfont\sffamily\LARGE\bfseries}}%
    \renewcommand\subsection{\@startsection{subsection}{2}{\z@}%
      {-2\baselineskip}%
      {\baselineskip}%
      {\normalfont\sffamily\Large\bfseries}}%
    \renewcommand\subsubsection{\@startsection{subsubsection}{3}{\z@}%
      {-\baselineskip}%
      {\baselineskip}%
      {\normalfont\sffamily\large\bfseries}}%
    \renewcommand\paragraph{\@startsection{paragraph}{4}{\z@}%
      {\baselineskip}%
      {-.5\baselineskip}%
      {\normalfont\sffamily\normalsize}}%
    \let\subsubsubsection\paragraph
    \renewcommand\subparagraph{\@startsection{subparagraph}{5}{\parindent}%
      {\baselineskip}%
      {-.1pt}%
      {\normalfont\sffamily\normalsize\itshape}}%
    \let\subsubsubsubsection\subparagraph
    \setcounter{secnumdepth}{-1}
    \let\@makefntext\oldfntext
    \renewcommand{\footnoterule}{\par\vspace*{1pc}}%
    \pagestyle{fancy}%
    \lhead{}\rhead{}\chead{}\lfoot{}\cfoot{\thepage}\rfoot{}%
    \renewcommand{\headrulewidth}{0pt}%
    \def\TeX{TeX}\def\LaTeX{LaTeX}\doublespacing\raggedbottom
    \typeout{Wordprocessor-style formatting}}}%
%
% Option to allow document to be set justified; added 2011-05-01
% in response to user queries
\newif\ifjustified
\DeclareOption{justified}{\justifiedtrue}
%
% Now apply the default options then read what's specified
%
\DeclareOption*{%
   \PackageWarning{uccthesis}{Unknown option ‘\CurrentOption’; 
     please Read The Fine Manual}%
}
\ProcessOptions\relax
\LoadClass[\class@mode,titlepage,oneside,\class@size,\class@paper]{report}
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% DECLARATIONS for this package and changes to report.cls
%
% Timestamp for drafts [0.7]
%
\newcounter{hours}
\newcounter{minutes}
\newcounter{temp}
\setcounter{hours}{\time}%
\setcounter{minutes}{\value{hours}}%
\divide\c@hours by60 % hours is now right
\setcounter{temp}{\value{hours}}%
\multiply\c@temp by60 % integer part to deduct
\addtocounter{minutes}{-\value{temp}}% make deduction
\newcommand{\now}{% \time is in minutes since midnight
  \ifnum\c@hours<10 0\fi\thehours:\ifnum\c@minutes<10 0\fi\theminutes}
%
% redefine \today to do it the European way round [1.4]
%
\def\today{\number\day\space\ifcase\month\or
  January\or February\or March\or April\or May\or June\or
  July\or August\or September\or October\or November\or December\fi
  \space\number\year}
\def\TODAY{\number\year{-}%
  \ifnum\month<10 0\fi\number\month{-}%
  \ifnum\day<10 0\fi\number\day}
%
% Define metadata (title, author, and date are given by LaTeX)
%
\def\@institution{\so{NATIONAL UNIVERSITY OF IRELAND, CORK}}
\newcommand{\institution}[1]{\gdef\@institution{\so{#1}}}
\def\@subtitle{\relax}
\newcommand{\subtitle}[1]{\gdef\@subtitle{#1}}
\def\@sponsor{\relax}
\newcommand{\sponsor}[1]{\gdef\@sponsor{#1}}
%\def\@sponsorlogo{\relax}
%\def\@sponsordummy{\relax}
%\def\@sponsorpreamble{\relax}
%\newcommand*{\sponsorlogo}[2][\relax]{%
%  \ifthenelse{\equal{\relax}{#1}}{%
%    \gdef\@sponsorlogo{#2}}{%
%    \gdef\@sponsorpreamble{#1}%
%    \gdef\@sponsordummy{#2}}}
\def\@supervisor{[Name not supplied]}
\def\@supervisorname{Supervisor}
\newcommand{\supervisor}[1]{\gdef\@supervisor{#1}}
\newcommand{\supervisors}[1]{\gdef\@supervisor{#1}%
  \def\@supervisorname{Supervisors}}
\def\@prof{[Name not supplied]}
\newcommand{\professor}[1]{\gdef\@prof{#1}}
\def\@quals{\relax}
\newcommand{\qualifications}[1]{\gdef\@quals{#1}}%
%
% Formal submission 
%
\def\@submission{%
  \ifthesis
    \if\@hospital\relax
    \else\par\textsc{\@hospital}%
    \fi
    \if\@school\relax
    \else\par\textsc{\@school}%
    \fi
    \if\@department\relax
    \else\par\textsc{\@department}%
    \fi
  \fi
  \if\@institute\relax
  \else\par\textsc{\@institute}%
  \fi
  \if\@centre\relax
  \else\par\textsc{\@centre}%
  \fi
  \if\@unit\relax
  \else\par\textsc{\@unit}%
  \fi
}
%
\def\@volume{\relax}
\def\@maxvolumes{\relax}
\def\volume #1 of #2 {\gdef\@volume{#1}\gdef\@maxvolumes{#2}}
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% PACKAGES
%
% textwidth=34pc,textheight=56pc,
\RequirePackage[margin=2.5cm,left=4cm,headheight=22pt]{geometry}
%
% Set the encoding right for UTF-8
%
\ifcjk
  \RequirePackage[encapsulated]{CJK}
  \newcommand{\cjktext}[1]{\begin{CJK}{UTF8}{min}#1\end{CJK}}
\fi
\RequirePackage{ucs}
\RequirePackage[utf8x]{inputenc}
\PrerenderUnicode{–}
\PrerenderUnicode{š}
\PrerenderUnicode{ć}
\PrerenderUnicode{Å}
\RequirePackage[T1]{fontenc}
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Other packages:
%	array		for tabular typesetting
%       ccaption        to make captions raggedright
%	fancyhdr	to get the running heads right
%       float           for adding sidebars
%       footmisc        for hanging indentation on footnotes
%	graphicx	for illustrations
%	ifthen		for makin decisions
%	lmodern		Updated Computer Modern
%	makeidx		for indexes
%	parskip		for spaced paragraphs
%	setspace	for line-and-a-half spacing
%	soul		for special underlining and spacing
%	textcomp	publishers symbols
%	ucc		college crest
%       ulem            for underlining and strike-out
%	url		formatting URLs
%	xcolor		for colour
%
\RequirePackage{lmodern}
\RequirePackage{makeidx,url,graphicx,array,fancyhdr,calc}
\RequirePackage{soul,textcomp,setspace,parskip,float}
\RequirePackage[normalem]{ulem}
\RequirePackage[bottom,ragged]{footmisc}
\RequirePackage{ccaption}
\captionstyle{\raggedright}%
%\RequirePackage[logonly]{trace}
\RequirePackage[svgnames]{xcolor}
\newcommand{\T}[2]{{\fontencoding{T1}\selectfont#2}}
%
% Fix fonts: we want normal bold, not bold extended
%
\AtBeginDocument{%
  \def\bfdefault{b}
}
%
% Portions of the page, maxima for various floats
%
% from http://www.tex.ac.uk/cgi-bin/texfaq2html?label=floats
\renewcommand{\topfraction}{.85}
\renewcommand{\bottomfraction}{.7}
\renewcommand{\textfraction}{.15}
\renewcommand{\floatpagefraction}{.75}
\renewcommand{\dbltopfraction}{.75}
\renewcommand{\dblfloatpagefraction}{.75}
\setcounter{topnumber}{9}
\setcounter{bottomnumber}{9}
\setcounter{totalnumber}{20}
\setcounter{dbltopnumber}{9}
% Specify tighter letterspacing
\sodef\so{}{.1em}{.5em plus.5em}{1em plus1em}
%
% define the title page here (we have \@titlepagetrue)
%
\newif\ifmadetitle
\renewcommand\maketitle{%
  \begin{titlepage}%
    \null\vfill
    \begin{center}%
% Requirement: title
      {\huge \@title \par}%
% Optional: subtitle
      \if\@subtitle\relax\else
        \vspace{2pc}{\LARGE\@subtitle\par}%
      \fi
% Optional: volumes 
      \if\@volume\relax\else
        \vspace{2pc}{\large Volume \@volume\ of \@maxvolumes\par}%
      \fi
      \vspace*{2pc}%
% Requirement: author: changed to LARGE 2011-04-07
      {\LARGE\@author%
% Option: qualifications
	\if\@quals\relax\else
          \\{\scshape \@quals}%
        \fi
	\par}%
      \vfill
      {\Large\UCCcrest}
      \par\medskip
% Requirement: institution
      \@institution\par
      \vspace*{.5pc}%
% Requirement: college/faculty/department
      \@submission\par
      \vspace*{1.5pc}%
% Requirement: grade of degree
      \ifthesis
        \textbf{\large Thesis submitted for the degree of \\\@degree}\par
      \fi
      \vspace*{1.5pc}%
% Requirement: date
      {\@date \par}%
      \vspace*{2pc}%
      \ifthesis
        \begin{tabular}{%
            >{\raggedleft\arraybackslash}p{.45\columnwidth}%
	    >{\raggedright\arraybackslash}p{.45\columnwidth}}
% Requirement: supervisor, optional dept head
	    \@supervisorname:        &
            \begin{tabular}[t]{@{}l@{}}\@supervisor\end{tabular}
          \if\@prof\relax
	  \else \\[.5pc]
            Head of Department/School:&\@prof
          \fi
        \end{tabular}\par\vfill%
      \else
        \iftermpaper
% Requirement: just supervisor
          Supervisor:\quad\@supervisor\par
        \fi
      \fi
      \vspace*{2pc}%
% Option: Sponsor
      \if\@sponsor\relax\else
        \vfill
%        \if\@sponsorlogo\relax
%          \if\@sponsordummy\relax\else
%             \begingroup\@sponsorpreamble
%             \fcolorbox{Violet}{CornflowerBlue}{%
%               \begin{tabular}{c}%
%                 \@sponsordummy
%               \end{tabular}}\par\endgroup
%             \vspace{6pt}%
%          \fi
%        \else
%          \includegraphics[width=1in]{\@sponsorlogo}%
%          \par\vspace*{6pt}%
%        \fi
	Research supported by \@sponsor\par
      \fi
    \end{center}%
    \setcounter{page}{0}%
    \thispagestyle{empty}%
  \end{titlepage}%
  \setcounter{footnote}{0}%
  \global\let\thanks\relax
  \global\let\maketitle\relax
  \global\let\@thanks\@empty
  \global\let\title\relax
  \global\let\author\relax
  \global\let\date\relax
  \global\let\and\relax
  \pagenumbering{roman}%
  \tableofcontents
  \listoffigures
  \listoftables
  \ifthesis
    \clearpage
    \null\vfill\markboth{}{}%
% Wording from Michelle Nelson, 2011-04-07
    \begingroup% \centering removed 2011-04-28
    I, \@author, certify that this thesis is my own work and I have
    not obtained a degree in this university or elsewhere on the basis
    of the work submitted in this thesis.
    \par\flushright\@author\par\endgroup
    \vfill
  \fi
  \madetitletrue}
% end of maketitle
\setcounter{secnumdepth}{5}
\setcounter{tocdepth}{3}
%
% New in frontmatter
%
\newenvironment{dedication}{%
  \ifmadetitle\else\maketitle\fi
  \clearpage\null\vfill
  \thispagestyle{empty}%
%  \addcontentsline{toc}{section}{Dedication}{}%
  \begin{center}}{%
  \end{center}\vfill}
\newenvironment{acknowledgements}{%
  \ifmadetitle\else\maketitle\fi
  \clearpage
  \section*{\ackname\@mkboth{\ackname}{\ackname}}%
  \addcontentsline{toc}{section}{\ackname}}{}%
\newcommand{\prelim}[1]{%
  \ifmadetitle\else\maketitle\fi
  \clearpage
  \section*{#1\@mkboth{{#1}}{{#1}}}%
  \addcontentsline{toc}{section}{#1}}
% fix silly description labels
\renewcommand*\descriptionlabel[1]{%
  \hspace\labelsep\bfseries\parbox{\columnwidth - 6pt}{\leavevmode\null\quad
    \fontseries{b}\selectfont#1 \vrule depth.5ex width0pt}}
\renewcommand\part{%
  \if@openright
    \cleardoublepage
  \else
    \clearpage
  \fi
  \thispagestyle{plain}%
  \if@twocolumn
    \onecolumn
    \@tempswatrue
  \else
    \@tempswafalse
  \fi
  \null\vfil
  \ifnum\c@part=0 \pagenumbering{arabic}\fi
  \secdef\@part\@spart}
\renewcommand{\chapter}{%
  \if@openright\cleardoublepage\else\clearpage\fi
  \thispagestyle{plain}%
  \global\@topnum\z@
  \ifnum\c@chapter=0 
    \ifnum\c@part=0 
      \ifthenelse{\equal{\@chapapp}{\chaptername}}{\pagenumbering{arabic}}{}%
    \fi
  \fi
  \@afterindentfalse
  \secdef\@chapter\@schapter}
% fix silly identation on quotations
\renewenvironment{quotation}[1][\relax]{%
  \gdef\@quotcite{#1}%
  \list{}{%
    \listparindent 1.5em%
    \itemindent    \listparindent
    \rightmargin   \leftmargin
    \parsep        \z@ \@plus\p@}%
  \item\relax\small\noindent\raggedright\ignorespaces
}{%
  \if\@quotcite\relax\else
    \item\relax\footnotesize\flushright
      \expandafter\@opttest\@quotcite\sentinel
      \ifoptarg\expandafter\cite\@quotcite
      \else\cite{\@quotcite}\fi\par
  \fi
  \endlist}
% test for option in optional argument
\newif\ifoptarg
\def\@opttest#1#2\sentinel{\ifx[#1\optargtrue\else\optargfalse\fi}
% fix captions
\setlength{\abovecaptionskip}{.5\baselineskip}
\setlength{\belowcaptionskip}{.5\baselineskip}
% floats
\floatstyle{plaintop}
\restylefloat{table}
\restylefloat{figure}
% ToC
\renewcommand\tableofcontents{%
  \clearpage\section*{\contentsname
    \@mkboth{\contentsname}{\contentsname}}%
  \@starttoc{toc}%
}
\renewcommand\listoffigures{%
  \clearpage\section*{\listfigurename
    \@mkboth{\listfigurename}{\listfigurename}}%
  \addcontentsline{toc}{section}{\listfigurename}%
  \@starttoc{lof}%
}
\renewcommand\listoftables{%
  \clearpage\section*{\listtablename
    \@mkboth{\listtablename}{\listtablename}}%
  \addcontentsline{toc}{section}{\listtablename}%
  \@starttoc{lot}%
}
\renewenvironment{theindex}{%
  \clearpage
  \columnseprule \z@
  \columnsep 35\p@
  \twocolumn[\section*{\indexname}]%
  \@mkboth{\indexname}{\indexname}%
  \parindent\z@
  \parskip\z@ \@plus .3\p@\relax
  \let\item\@idxitem}%
 {\clearpage}
%
% Odds 'n' sods
%
\renewcommand\@pnumwidth{2em}% added 2011-04-28
\def\Frac#1#2{\raise.75ex\hbox{{\tiny#1}}%
  \hspace{-.125ex}/\hspace{-.125ex}%
  \lower.25ex\hbox{{\tiny#2}}}
\def\inch{\ifmmode{}^{\prime\prime}\else${}^{\prime\prime}$\fi}
\newcommand{\me}{\@author}
\newcommand{\signature}[1][\relax]{%
  \begin{flushright}\small\me\\
    \if#1\relax\else#1, \fi\@date
  \end{flushright}}
\newcommand{\tm}{\raisebox{2pt}{\scriptsize\textsf{\texttrademark}}}
\newcommand{\etc}{etc.\ }
\newcommand{\eg}{e.g.\ }
\newcommand{\ie}{i.e.\ }
% better strikeout
\def\sout{\bgroup \ULdepth=-.75ex \ULset}
%
% Abstract
%
\renewenvironment{abstract}{% Don't use titlepage, it resets the pageno!
  \ifmadetitle\else\maketitle\fi
  \clearpage
  \@beginparpenalty\@lowpenalty
  \section*{\abstractname}%
  \addcontentsline{toc}{section}{\abstractname}%
  \markboth{\abstractname}{\abstractname}%
}{%
  \clearpage
}
%
% More Odds'n'Sods
%
\hyphenation{%
Regi-strar 
Sebas-tian 
analy-sis
Analy-sis
bre-vi-ty 
chan-ges 
de-fi-ning 
docu-ment 
epex-e-ge-ses
exa-mined
helico-pter
hier-ar-chy 
inter-linea-ted
lexi-cal 
manu-script
re-fe-ren-ces
re-prin-ted 
smal-ler 
spe-ci-fy 
sub-con-sci-ous-ly 
tech-no-logy
typo-gra-phy
No-men-cla-ture
}
\let\foreign\emph
\newcommand{\marginal}[1]{%
  \marginpar{\sffamily\tiny\raggedright#1\par}}
%
% Fancy headings, need flattened title
%
\newcommand{\@flattitle}{{\let\\\thinspace\@title}}
\pagestyle{fancy}
\fancypagestyle{plain}
\fancyhf{}
\lhead{\ifthenelse{\equal{\leftmark}{\rightmark}}{}{%
    \footnotesize\scshape\parbox[b]{.475\columnwidth}{%
      \raggedright{\let\\\space\leftmark}}}}
\chead{}
\rhead{\footnotesize\itshape\parbox[b]{.475\columnwidth}{%
    \flushright{\let\\\space\rightmark}}}
\lfoot{\fancyplain{}{\footnotesize\parbox[t]{.475\columnwidth}{%
      \raggedright{\let\\\space\@title}}}}% zapped \\ 2011-04-28
\cfoot{\fancyplain{\small\thepage}{\small\thepage}}
\rfoot{\fancyplain{}{\footnotesize\@author}}
\renewcommand{\chaptermark}[1]{\markboth{\thechapter.\ #1}{}}
\renewcommand{\sectionmark}[1]{\markright{\thesection\ #1}{}}
\addtolength{\headheight}{10pt}
\renewcommand{\headrulewidth}{0pt}
%
\newcommand{\vstrut}{\vrule height1.2em depth.6667ex width0pt}%
\newcommand{\textbeta}{\ensuremath{\beta}}
%
\onehalfspacing % Required: line-and-a-half spacing
\makeindex
\raggedbottom
\ifjustified\else\raggedright\fi% added 2011-05-01 
\endinput
%%
%% End of file `uccthesis.cls'.
