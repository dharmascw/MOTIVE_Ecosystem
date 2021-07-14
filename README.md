# Projects MOTIVE Ecosystem

MOTIVE Ecosystem 전문가시스템 설치

1. 내 컴퓨터에 R(R4.0.5)프로그램을 다운받아 설치한다.

2. 내 컴퓨터에 Rtools(Rtools40)를 다운받아 설치한다.

3. https://github.com/dharmascw/MOTIVE_Ecosystem 에서 R(R4.0.5) library(4.0)을 사용자의 R library 공간(예, C:\Users\<사용자이름>\Documents\R\win-library)에 다운받아 Zip 파일을 푼다.

4. 내 컴퓨터의 제어판 시스템의 고급시스템설정에서 환경변수 Path에 R(C:/Program Files/R/R-4.0.5/bin)과 Rtools(./Rtools/usr/bin)의 실행파일 위치를 추가한다.

5. https://github.com/dharmascw/MOTIVE_Ecosystem 에서 최근 프로그램 파일(예, MOTIVE_Ecosystem_20210611.zip)을 나의 시스템 공간(예, C:/MOTIVE_System/)에 다운받아 Zip 파일을 푼다. 그리고 이름은 PROGRAM(사용자 명명 가능)으로 설정한다.

6. https://github.com/dharmascw/MOTIVE_Ecosystem 에서 데이터 파일(예, DATA_MOTIVE_sample_20210713.zip)을 나의 시스템 공간(예, C:/MOTIVE_System/)에 다운받아 Zip 파일을 푼다. 그리고 이름은 DATA(사용자 명명 가능)로 설정한다.

5. install_shiny.R을 실행한다. (MOTIVE 시스템을 실행하기 위해서는 먼저 shiny와 SDMTools package가 설치되어야 한다.)
    - R(R4.0.5)프로그램을 실행한다.
    - R(R4.0.5)>source(<저장폴더>/install_shiny.R")
    - R(R4.0.5)프로그램을 종료한다.

6. 작업환경을 셜정한다.
   1) System_Environment.txt: 언어, 작업공간, 데이터 위치. 종정보, 옵션파일 등 원하는 작업환경을 설정한다.
   2) option_lists_ENG/KOR: 한글 또는 영문메뉴의 옵션들을 설정한다. 
   3) variable_lists: 한글 또는 영문 변수들의 이름을 설정한다.

6. ecosystem.bat을 실행한다.
