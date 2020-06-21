# Projects MOTIVE Ecosystem

설치방법

1. 내 컴퓨터에 R(R3.6.1)프로그램을 다운받아 설치한다.
2. 내 컴퓨터에 Rtools 를 다운받아 설치한다.
3. 내 컴퓨터의 제어판 시스템의 고급시스템설정에서 환경변수 Path에 R과 Rtools의 실행파일 위치를 추가한다.
4. https://github.com/dharmascw/MOTIVE_Ecosystem 에서 전체 파일들을 Zip으로 나의 작업공간에 다운받는다.
5. Zip 파일을 푼다.
6. R(R3.6.1)프로그램을 실행하여 install_shiny.R을 실행한다. (
   : ecosystem을 실행하기 위해서는 먼저 shiny와 SDMTools package가 설치되어야 한다.
   >source(""해당폴더"/install_shiny.R")
   R(R3.6.1)프로그램을 종료한다.
7. 사용자환경 파일을 이용하여 원하는 환경을 설정한다.
   System_Environment.txt: 언어, 작업공간, 데이터 위치. 종정보, 옵션파일들을 지정한다.
   option_lists_ENG/KOR: 한글 또는 영문메뉴의 옵션들을 설정한다.
   variable_lists: 한글 또는 영문 변수들의 이름을 설정한다.
8. ecosystem.bat을 실행한다.