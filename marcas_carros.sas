/* importando os dados do excel */

/*  AAS 1 */

%web_drop_table(WORK.aas1);

FILENAME REFFILE '/home/u64202552/banco_vagas.xlsx';

PROC IMPORT DATAFILE=REFFILE
	DBMS=XLSX
	OUT=WORK.aas1;
	SHEET= 'AAS_1';
	GETNAMES=YES;
RUN;

PROC CONTENTS DATA=WORK.aas1; RUN;

%web_open_table(WORK.aas1);

/* Sis 1 */

%web_drop_table(WORK.sis1);


FILENAME REFFILE '/home/u64202552/banco_vagas.xlsx';

PROC IMPORT DATAFILE=REFFILE
	DBMS=XLSX
	OUT=WORK.sis1;
	SHEET= 'Sistematica_1';
	GETNAMES=YES;
RUN;

PROC CONTENTS DATA=WORK.sis1; RUN;

%web_open_table(WORK.sis1);

/*  AAS 2 */

%web_drop_table(WORK.aas2);

FILENAME REFFILE '/home/u64202552/banco_vagas.xlsx';

PROC IMPORT DATAFILE=REFFILE
	DBMS=XLSX
	OUT=WORK.aas2;
	SHEET= 'AAS_2';
	GETNAMES=YES;
RUN;

PROC CONTENTS DATA=WORK.aas2; RUN;

%web_open_table(WORK.aas2);

/* Sis 2 */

%web_drop_table(WORK.sis2);


FILENAME REFFILE '/home/u64202552/banco_vagas.xlsx';

PROC IMPORT DATAFILE=REFFILE
	DBMS=XLSX
	OUT=WORK.sis2;
	SHEET= 'Sistematica_2';
	GETNAMES=YES;
RUN;

PROC CONTENTS DATA=WORK.sis2; RUN;

%web_open_table(WORK.sis2);

/* tabela de frequência das marcas por amostra */

data aas1 ;

	set WORK.AAS1 (keep = Marca UF Município "Valor Venal"n);

run;

data aas2 ;

	set WORK.AAS2 (keep = Marca UF Município "Valor Venal"n);
	
run;

data sis1 ;

	set WORK.SIS1 (keep = Marca UF Município "Valor Venal"n);
	
run;

data sis2 ;

	set WORK.SIS2 (keep = Marca UF Município "Valor Venal"n);
	
run;

proc freq data=aas1 noprint;
  tables Marca / out=freq_aas1;
run;

proc freq data=aas2 noprint;
  tables Marca / out=freq_aas2;
run;

proc freq data=sis1 noprint;
  tables Marca / out=freq_sis1;
run;

proc freq data=sis2 noprint;
  tables Marca / out=freq_sis2;
run;

data freq_aas1; set freq_aas1(rename=(count=AAS1)); keep Marca AAS1; run;
data freq_aas2; set freq_aas2(rename=(count=AAS2)); keep Marca AAS2; run;
data freq_sis1; set freq_sis1(rename=(count=SIS1)); keep Marca SIS1; run;
data freq_sis2; set freq_sis2(rename=(count=SIS2)); keep Marca SIS2; run;

/* juntar todas as tabelas pela marca */
proc sort data=freq_aas1; by Marca; run;
proc sort data=freq_aas2; by Marca; run;
proc sort data=freq_sis1; by Marca; run;
proc sort data=freq_sis2; by Marca; run;

data freq_marcas_final;
  merge freq_aas1 freq_sis1 freq_aas2 freq_sis2;
  by Marca;
  array amostras[*] AAS1 SIS1 AAS2 SIS2;

  do i = 1 to dim(amostras);
    if amostras[i] = . then amostras[i] = 0;
  end;

  Total_Frequencia = sum(of AAS1 AAS2 SIS1 SIS2);
  drop i;
run;

data freq_marcas_final_filtrada;
  set freq_marcas_final;
  if strip(Marca) ne "";
run;

proc sql;
  select 
    sum(AAS1), sum(SIS1), sum(AAS2), sum(SIS2), sum(Total_Frequencia)
  into :total_aas1, :total_aas2, :total_sis1, :total_sis2, :total_geral
  from freq_marcas_final_filtrada;
quit;

data linha_total;
  length Marca $100;
  Marca = "TOTAL";
  AAS1 = &total_aas1;
  SIS1 = &total_sis1;
  AAS2 = &total_aas2;
  SIS2 = &total_sis2;
  Total_Frequencia = &total_geral;
run;

data freq_com_total;
  length Marca $100;
  set freq_marcas_final_filtrada linha_total;
run;

/* print da tabela */
proc print data=freq_com_total noobs label;
  title "Frequência de Marcas por Amostra";
  label 
    Marca = "Marca"
    AAS1 = "Amostra AAS1"
    SIS1 = "Amostra SIS1"
    AAS2 = "Amostra AAS2"
    SIS2 = "Amostra SIS2"
    Total_Frequencia = "Frequência Total";
run;

/* salvando a tabela em excel*/

ods excel file="/home/u64202552/sasuser.v94/Trabalho_Amostragem/grafico_tabelas/freq_marcas_formatada.xlsx";

proc print data=freq_com_total noobs label;
  title "Frequência de Marcas por Amostra";
  label 
    Marca = "Marca"
    AAS1 = "Amostra AAS1"
    SIS1 = "Amostra SIS1"
    AAS2 = "Amostra AAS2"
    SIS2 = "Amostra SIS2"
    Total_Frequencia = "Frequência Total";
run;

ods excel close;

/* gráficos */
proc sgplot data=freq_aas1;
  title "Gráfico de Barras da Quantidade de Veículos por Marca - Amostra Aleatória Simples 1";
  vbar Marca / response=AAS1 datalabel categoryorder=respdesc;
  yaxis label="Frequência";
  xaxis label="Marca";
run;

proc sgplot data=freq_sis1;
  title "Gráfico de Barras da Quantidade de Veículos por Marca - Amostra Sistemática 1";
  vbar Marca / response=SIS1 datalabel categoryorder=respdesc;
  yaxis label="Frequência";
  xaxis label="Marca";
run;

proc sgplot data=freq_aas2;
  title "Gráfico de Barras da Quantidade de Veículos por Marca - Amostra Aleatória Simples 2";
  vbar Marca / response=AAS2 datalabel categoryorder=respdesc;
  yaxis label="Frequência";
  xaxis label="Marca";
run;


proc sgplot data=freq_sis2;
  title "Gráfico de Barras da Quantidade de Veículos por Marca - Amostra Sistemática 2";
  vbar Marca / response=SIS2 datalabel categoryorder=respdesc;
  yaxis label="Frequência";
  xaxis label="Marca";
run;

/* salvando os gráficos */
ods listing gpath="/home/u64202552/sasuser.v94/Trabalho_Amostragem/grafico_tabelas" image_dpi=300;

ods graphics / reset=all imagename="grafico_aas1" imagefmt=png;
title "Amostra Aleatória Simples 1";
proc sgplot data=freq_aas1;
  vbar Marca / response=AAS1 datalabel categoryorder=respdesc;
  yaxis label="Frequência";
  xaxis label="Marca";
run;

ods graphics / imagename="grafico_aas2";
title "Amostra Aleatória Simples 2";
proc sgplot data=freq_aas2;
  vbar Marca / response=AAS2 datalabel categoryorder=respdesc;
  yaxis label="Frequência";
  xaxis label="Marca";
run;

ods graphics / imagename="grafico_sis1";
title "Amostra Sistemática 1";
proc sgplot data=freq_sis1;
  vbar Marca / response=SIS1 datalabel categoryorder=respdesc;
  yaxis label="Frequência";
  xaxis label="Marca";
run;

ods graphics / imagename="grafico_sis2";
title "Amostra Sistemática 2";
proc sgplot data=freq_sis2;
  vbar Marca / response=SIS2 datalabel categoryorder=respdesc;
  yaxis label="Frequência";
  xaxis label="Marca";
run;

ods listing close;