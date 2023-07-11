# MOTIVE Ecosystem 전문가시스템 설치

1. 내 컴퓨터에 R(R4.0.5)프로그램을 다운받아 설치한다.
2. 내 컴퓨터에 JAVA프로그램을 다운받아 설치한다.
3. 내 컴퓨터에 MOTIVE_Ecosystem.zip과 MOTIVE_projects.zip을 다운받아 설치한다.
4. 내 컴퓨터 root 디렉토리(C:/ 또는 D:/)에 Zip 파일을 푼다.
  
   1) C:/MOTIVE_Ecosystem
      - 4.0 (R user library)
      - DATA (Input data)
      - Projects-master (Program)
      ※ 향후 갱신된 프로그램은 https://github.com/dharmascw/MOTIVE_Ecosystem에서 최근 프로그램 파일(예, Projects-master_20220919.zip)을 나의 시스템 공간(예,        C:/MOTIVE_System/)에 다운받아 Projects-master를 설치하고 System_Environment.txt에서 환경을 재설정한다.
  
   2) C:\MOTIVE_projects
       - proj_test (Project sample)

4. 내 컴퓨터의 제어판 시스템의 고급시스템설정에서 환경변수를 설정하다.
   1) R_LIBS_USER 변수를 추가하고 C:/MOTIVE_Ecosystem/4.0를 설정한다.
	(R 사용자 library를 설정한다)
   2) R_USER 변수를 추가하고 C:/MOTIVE_projects를 설정한다.
	(R 사용자 디렉토리를 설정한다)
   3) Path에 C:/Program Files/R/R-4.0.5/x64/bin (R 실행파일 디렉토리)을 추가한다.

5. 작업환경을 설정한다.
   1) System_Environment.txt: 언어, 작업공간, 데이터 위치. 종정보, 옵션파일 등 원하는 작업환경을 설정한다.
      - option_lists_ENG/KOR: 한글 또는 영문메뉴의 옵션 
      - variable_lists: 한글 또는 영문 변수들의 이름
6. ecosystem.bat을 실행한다.
