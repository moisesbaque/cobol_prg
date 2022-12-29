       IDENTIFICATION       DIVISION.
       PROGRAM-ID.          DISTANCIA.
       AUTHOR.              MOISES.
       ENVIRONMENT          DIVISION.
       INPUT-OUTPUT         SECTION.
       DATA                 DIVISION.
       WORKING-STORAGE      SECTION.
      *> variaveis para calcular distâncias.
       01  lat1             COMP-2.
       01  long1            comp-2.
       01  lat2             comp-2.
       01  long2            comp-2.
       01  distancia-km     comp-2.
       01  distancia-m      comp-2.

      *> variaveis para converter angulos entre distâncias.

       01  distancia-result.
           02 filler            pic x(11)    value "Distancia: ".
           02 distancia-km-dsp  pic zzz,zz9.
           02 filler            pic x(6)    value " km".
           02 distancia-m-dsp   pic zzz,zz9.
           02 filler            pic x(7)    value " milhas".

       78  pi                   value 3.14159.

       78  raio-da-terra        value 6371. *> raio da Terra

       78  km-por-milha         value 1.609344.

       01  temp                 comp-2.

       PROCEDURE   DIVISION.
       main section.
      * ------------------------------------------
      *    geo Sao Paulo
           move "-23.550519" to temp
           compute lat1 = temp * (pi / 180)

           move "-46.633309" to temp
           compute long1 = temp * (pi / 180)
      *-------------------------------------------
      *    geo New York
           move "+40.7127837" to temp
           compute lat2 = temp * (pi / 180)

           move "-74.005941" to temp
           compute long2 = temp * (pi / 180)

           compute distancia-km = function acos(
               function sin(lat1) * function sin(lat2)
                       + function cos(lat1) * function cos(lat2)
                               * function cos(long2 - long1))
                       * raio-da-terra

           compute distancia-m = distancia-km / km-por-milha.

           compute distancia-km-dsp = distancia-km
           compute distancia-m-dsp  = distancia-m

           display distancia-result.

           stop run.
