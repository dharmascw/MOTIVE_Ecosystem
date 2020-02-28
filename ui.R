### JD Edition
library(shinythemes)
ST_Name <- "success"


shinyUI(

  dashboardPage(
    skin = "green",
    dashboardHeader(title = "MOTIVE Ecosystem"),
    dashboardSidebar(),
    dashboardBody(
      
      shinyjs::useShinyjs(),
      
      fluidPage( div(
        h4(SE_Name_System, style = "display: inline-block; color: white; font-size: 200%; margin-left: 20px; position: absolute; line-height: 8vh;"), 
        div( style = "display: inline-block; margin-left: 80%; margin-top: 10px; font-size: 110%; color: white;",
                  
#             a("   한국어", style = "cursor:pointer; margin-right: 5px; color: white;" ),
#             a("English", style = "cursor:pointer; margin-right: 5px; color: white; " ),
#             a("CONTACT US", style = "cursor:pointer; margin-right: 5px; color: white; " ),
#             a("LOGOUT", style = "cursor:pointer; margin-right: 5px; color: white;" )
            a(SE_Name_System_institute, style = "cursor:pointer; margin-right: 5px; color: ghostwhite;" )

        ),
        style = "background-image: url(eco_title.png); height: 10vh; position: relative;"),
        tags$script(HTML(
          'document.querySelector("body").classList.add("sidebar-collapse");'
        )),
        
        theme = shinytheme("yeti"),
        # shinythemes::themeSelector(),
        
        tags$head(tags$style(HTML('
                                  
                                  #SE_Dir_Project {
                                  display: inline;
                                  }
                                  
                                  #SDM_MO_SDM_run {
                                  width: 200px;
                                  height: 70px;
                                  font-size: 3rem;
                                  }
                                  
                                  #DM_MO_Action {
                                  width: 200px;
                                  height: 70px;
                                  }
                                  
                                  #CM_UI, #CS_UI, #PY_UI, #SYNC_UI {
                                  display: inline-block;
                                  }
                                  
                                  #CM_btn, #CS_btn, #PY_btn {
                                  color: #fff;
                                  background-color: #0080ff;
                                  }
                                  
                                  #SS_Analy_Box > div > div {
                                  padding-left: 0;
                                  padding-right: 15px;
                                  }
                                  
                                  #SS_Analy_Box > div > div:first-child,
                                  #SS_Analy_Box > div > div:last-child {
                                  width: 20%;
                                  }
                                  
                                  #SS_Analy_Box > div > div:nth-child(2),
                                  #SS_Analy_Box > div > div:nth-child(3) {
                                  width: 13%;
                                  }
                                  
                                  #SS_Analy_Box > div > div:nth-child(4) {
                                  width: 15%;
                                  }
                                  
                                  #SS_Analy_Box > div > div:nth-child(5) {
                                  width: 18%;
                                  }
                                  
                                  #CD_Summary, #CD_Histogram {
                                  display: inline-block;
                                  width: 40%;
                                  }
                                  
                                  ##CD_Histogram {
                                  width: 40%;
                                  }
                                  
                                  .fa-folder-open {
                                  color: #3498db;
                                  }
                                  
                                  .fa-pie-chart {
                                  color: #9b59b6;
                                  }
                                  
                                  .fa-table {
                                  color: #e74c3c;
                                  }
                                  
                                  .container-fluid > .tabbable > .nav-tabs {
                                  font-weight: bold;
                                  }
                                  '))),

#	fluidPage(h4(SE_Name_System),
	tags$hr(),

    setBackgroundColor("ghostwhite"),

	tabsetPanel(
		tabPanel(SE_Name,
			tabsetPanel(
				tabPanel(SE_Name_WE,
					fluidRow(column(6,
					tags$hr(),
						shinyDirButton("SE_Dir_Project", SE_Name_WE_Project, SE_Name_WE_Project),
						verbatimTextOutput("SE_Dir_Project", placeholder = TRUE)
					))
				),
				tabPanel(SE_Name_DE, 
					fluidRow(column(6,
						tags$hr(),
						shinyDirButton("SE_Dir_Climate", SE_Name_DE_Climate, SE_Name_DE_Climate),
						verbatimTextOutput("SE_Dir_Climate", placeholder = TRUE),
					
						shinyDirButton("SE_Dir_Link", SE_Name_DE_Link, SE_Name_DE_Link),
						verbatimTextOutput("SE_Dir_Link", placeholder = TRUE),
						
						shinyDirButton("SE_Dir_Species", SE_Name_DE_Species, SE_Name_DE_Species),
						verbatimTextOutput("SE_Dir_Species", placeholder = TRUE),
						
						tags$hr(),
						fileInput("SE_speciesinfo", SE_Name_DE_Species_Index,
							accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
							),
						verbatimTextOutput("SE_speciesindex", placeholder = TRUE),
						tags$hr(),                         
						fileInput("SE_speciesdata1", SE_Name_DE_Species_Location,
							accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
						), 
						verbatimTextOutput("SE_specieslocation", placeholder = TRUE)
					))
				)
			)
		),       

		tabPanel(SP_Name,
			tabsetPanel(
				tabPanel(SP_Name_Info,
					tags$head(
						# Include our custom CSS
						includeCSS("styles.css"),
						includeScript("gomap.js")
					),
					tags$hr(),
					fluidRow(
						column(6, DT::dataTableOutput("SP_Info")),
						column(6, leafletOutput("SP_Map", width = "500", height = "600"))
					)
#					fluidRow(
#						tags$hr(),
#						column(6, 
#							verbatimTextOutput("SP_Summary"),
#							plotOutput("SP_Histogram")
#						)
#					)
				),
				tabPanel(SP_Name_Location,
					tags$head(
						# Include our custom CSS
						includeCSS("styles.css"),
						includeScript("gomap.js")
					),
					tags$hr(),
					column(6, DT::dataTableOutput("SP_LOC_Info")),
					column(6, leafletOutput("SP_LOC_Map", width = "500", height = "600"))
				)             
			)
		),  

		tabPanel(LD_Name, fluid = TRUE,
			tags$hr(),
			sidebarLayout(
				sidebarPanel(width = 3, Fluid = TRUE,
					selectInput("LD_Variables", LD_Name_Variables,
						choices = LD_Name_Variables_list,
						selected = LD_Name_Variables_selected),

					# Input: Checkbox if file has header ----
					radioButtons("LD_Climate_model", LD_Name_Models,
						choices = LD_Name_Models_list,
						selected = LD_Name_Models_selected),

					# Input: Checkbox if file has header ----
					radioButtons("LD_Climate_scenario", LD_Name_Scenarios,
						choices = LD_Name_Scenarios_list,
						selected = LD_Name_Scenarios_selected),

					# Input: Checkbox if file has header ----
					radioButtons("LD_Project_year", LD_Name_Year,
						choices = LD_Name_Year_list,
						selected = LD_Name_Year_selected)
				),

				# Main panel for displaying outputs ----
				mainPanel(
					tabsetPanel(
						tabPanel(LD_Name_Map, 
							tags$head(
							# Include our custom CSS
							includeCSS("styles.css"),
							includeScript("gomap.js")
							),
							tags$hr(),
							column(6, leafletOutput("LD_Map", width = "800", height = "650"))
						),
						tabPanel(LD_Name_Summary,
							tags$hr(),
							column(10, verbatimTextOutput("LD_Summary")),
							column(10, plotOutput("LD_Histogram"))
						)
					)
				)
			)
		),  

		tabPanel(CD_Name, fluid = TRUE,
			tags$hr(),
            sidebarLayout(
				sidebarPanel(width = 3, Fluid = TRUE,
					selectInput("CD_Variables", CD_Name_Variables,
						choices = CD_Name_Variables_list,
						selected = CD_Name_Variables_selected),

					# Input: Checkbox if file has header ----
					radioButtons("CD_Climate_model", CD_Name_Models,
						choices = CD_Name_Models_list,
						selected = CD_Name_Models_selected),
	
					# Input: Checkbox if file has header ----
					radioButtons("CD_Climate_scenario", CD_Name_Scenarios,
						choices = CD_Name_Scenarios_list,
						selected = CD_Name_Scenarios_selected),
	
					# Input: Checkbox if file has header ----
					radioButtons("CD_Project_year", CD_Name_Year,
						choices = CD_Name_Year_list,
						selected = CD_Name_Year_selected)
				),
	
				# Main panel for displaying outputs ----
				mainPanel(
					tabsetPanel(
						tabPanel(CD_Name_Map, 
							tags$head(
								# Include our custom CSS
								includeCSS("styles.css"),
								includeScript("gomap.js")
							),
							tags$hr(),
							column(6, leafletOutput("CD_Map", width = "800", height = "650"))
						),
						tabPanel(CD_Name_Summary,
							tags$hr(),
							column(10, verbatimTextOutput("CD_Summary")),
							column(10, plotOutput("CD_Histogram"))
						)
					)
				)
            )
        ),  
          
		tabPanel(SDM_Name,
			tabsetPanel(
				tabPanel(SDM_Name_Model,
					tabsetPanel(
						tabPanel(SDM_Name_Model_Species,
							fluidRow(
								tags$hr(),
								column(6, DT::dataTableOutput("SDM_SP_Info")),
								column(4, verbatimTextOutput("SDM_SP_Selection"))
							)
						),
						tabPanel(SDM_Name_Model_Projection,
							tags$hr(),
							fluidRow(
								# Sidebar panel for inputs ----
								sidebarPanel(width = 4,
									# Input: Checkbox if file has header ----
									checkboxGroupInput("SDM_MO_Climate_model", SDM_Name_CD_Models,
										choices = c(SDM_Name_CD_Models_list),
										selected = SDM_Name_CD_Models_selected),

									# Input: Checkbox if file has header ----
									checkboxGroupInput("SDM_MO_Climate_scenario", SDM_Name_CD_Scenarios,
										choices = c(SDM_Name_CD_Scenarios_list),
										selected = SDM_Name_CD_Scenarios_selected),

									# Input: Checkbox if file has header ----
									checkboxGroupInput("SDM_MO_Protect_year", SDM_Name_CD_Year,
										choices = c(SDM_Name_CD_Year_list),
										selected = SDM_Name_CD_Year_selected)
								)	
							)
						),
						tabPanel(SDM_Name_Model_Variable,  
							tags$hr(),
							fluidRow(
								# Sidebar panel for inputs ----
								sidebarPanel(width = 5,
									checkboxGroupInput("SDM_MO_Variables", SDM_Name_CD_Variables,
										choices = c(SDM_Name_CD_Variables_list),
										selected = SDM_Name_CD_Variables_selected)
								)
							)
						),
						tabPanel(SDM_Name_Model_SDM,  
							tags$hr(),
							fluidRow(
								# Sidebar panel for inputs ----
								sidebarPanel(width = 3,
                  tags$p("Data Options:"),
#                  uiOutput("DM_MO_DM_envChgSteps"),
#                  uiOutput("DM_MO_DM_envChgSteps"),
#                  uiOutput("DM_MO_DM_envChgSteps"),
#                  uiOutput("DM_MO_DM_envChgSteps"),
                  selectInput("BIOMOD_eval.resp.var", "BIOMOD_eval.resp.var",
                    choices = NULL,
                    selected = NULL),
                  selectInput("BIOMOD_eval.expl.var", "BIOMOD_eval.expl.var",
                    choices = NULL,
                    selected = NULL),
                  selectInput("BIOMOD_eval.resp.xy", "BIOMOD_eval.resp.xy",
                    choices = NULL,
                    selected = NULL),
                  sliderInput("BIOMOD_PA.nb.rep", label = "BIOMOD_PA.nb.rep", min = 0, 
                    max = 10, step = 1, value = 1),
                  sliderInput("BIOMOD_PA.nb.absences", label = "BIOMOD_PA.nb.absences", min = 0, 
                    max = 10000, step = 1000, value = 1000),
                  selectInput("BIOMOD_PA.strategy", "BIOMOD_PA.strategy",
                    choices = c("random", "sre", "disk", "user.defined"),
                    selected = "random"),
                  sliderInput("BIOMOD_PA.dist", label = "BIOMOD_PA.dist Range (m) for PA disk option", min = 10000, 
                    max = 200000, step = 10000, value = c(10000, 100000)),
                  sliderInput("BIOMOD_PA.sre.quant", label = "BIOMOD_PA.sre.quant", min = 0, 
                    max = 1, step = 0.025, value = 0.025),
                  selectInput("BIOMOD_PA.table", "BIOMOD_PA.table for PA sre option",
                    choices = NULL,
                    selected = NULL),
                  checkboxInput("BIOMOD_na.rm", "BIOMOD_na.rm", TRUE)
                ),

								sidebarPanel(width = 3,
								  tags$p("SDM Modeling Optios:"),
								  sliderInput("BIOMOD_NbRunEval", label = "BIOMOD_NbRunEval", min = 0, 
                    max = 10, step = 1, value = 1),
                  sliderInput("BIOMOD_DataSplit", label = "BIOMOD_DataSplit", min = 0, 
                    max = 100, step = 10, value = 100),
								  selectInput("BIOMOD_Yweights", "BIOMOD_Yweights",
                    choices = NULL,
                    selected = NULL),
								  sliderInput("BIOMOD_VarImport", label = "BIOMOD_VarImport", min = 0, 
                    max = 10, step = 1, value = 5),
								  checkboxGroupInput("BIOMOD_models.eval.meth", "BIOMOD_models.eval.meth",
                    choices = c("ROC", "TSS", "KAPPA"),  #, "FAR", "SR", "ACCURANCY", "BIAS", "POD", "CSI", "ETS"),
                    selected = "ROC"),
								  br(),
								  checkboxInput("BIOMOD_SaveObj", "BIOMOD_SaveObj", TRUE),
								  checkboxInput("BIOMOD_rescal.all.models", "BIOMOD_rescal.all.models", TRUE),
								  checkboxInput("BIOMOD_do.full.models", "BIOMOD_do.full.models", TRUE),
								  tags$hr(),
								  tags$p("Model Projection Options:"),
								  selectInput("BIOMOD_selected.models", "BIOMOD_selected.models",
								               choices = "all",
								               selected = "all"),
								  checkboxGroupInput("BIOMOD_binary.meth", "BIOMOD_binary.meth",
								                     choices = c("ROC", "TSS", "KAPPA"),  #, "FAR", "SR", "ACCURANCY", "BIAS", "POD", "CSI", "ETS"),
								                     selected = "ROC"),
								  br(),
								  checkboxInput("BIOMOD_compress", "BIOMOD_compress", FALSE),
								  checkboxInput("BIOMOD_build.clamping.mask", "BIOMOD_build.clamping.mask", FALSE),
								  selectInput("BIOMOD_output.format", "BIOMOD_output.format",
								              choices = ".img",
								              selected = ".img"),
								  checkboxInput("BIOMOD_do.stack", "BIOMOD_do.stack", TRUE)
								             
								),
								sidebarPanel(width = 3,
								  tags$p("Ensemble Modeling Options:"),
                  selectInput("EM_chosen.models", "EM_chosen.models",
                    choices = "all",
                    selected = "all"),
                  selectInput("EM_em.by", "EM_em.by",
                    choices = c("PA_dataset+repet", "PA_dataset+algo", "PA_dataset", "algo", "all"),
                    selected = "PA_dataset+repet"),
                  selectInput("EM_eval.metric", "EM_eval.metric",
                    choices = "all",
                    selected = "all"),
                  selectInput("EM_eval.metric.quality.threshold", "EM_eval.metric.quality.threshold",
                              choices = NULL,
                              selected = NULL),
                  checkboxGroupInput("EM_models.eval.meth", "EM_models.eval.meth",
                                     choices = c("ROC", "TSS", "KAPPA"),  #, "FAR", "SR", "ACCURANCY", "BIAS", "POD", "CSI", "ETS"),
                                     selected = "ROC"),
                  br(),
                  checkboxInput("EM_prob.mean", "EM_prob.mean", TRUE),
                  checkboxInput("EM_prob.cv", "EM_prob.cv", FALSE),
                  checkboxInput("EM_prob.ci", "EM_prob.ci", FALSE),
                  sliderInput("EM_prob.ci.alpha", label = "EM_prob.ci.alpha", min = 0, 
                              max = 1, step = 0.05, value = 0.05),
                  checkboxInput("EM_prob.median", "EM_prob.median", FALSE),
                  checkboxInput("EM_committee.averaging", "EM_committee.averaging", FALSE),
                  checkboxInput("EM_prob.mean.weight", "EM_prob.mean.weight", TRUE),
                  selectInput("EM_prob.mean.weight.decay", "EM_prob.mean.weight.decay",
                              choices = "proportional",
                              selected = "proportional"),
                  sliderInput("EM_VarImport", label = "EM_VarImport", min = 0, 
                              max = 10, step = 1, value = 0)
								),
								sidebarPanel(width = 3,
									checkboxGroupInput("SDM_MO_SDM_model", SDM_Name_models,
										choices = c(SDM_Name_models_list),
										selected = c(SDM_Name_models_selected)
									),
									tags$hr(), 
									checkboxInput("SDM_MO_SDM_EMmodel", label = SDM_Name_EMmodels, value = FALSE),
									tags$hr(),
									shinyDirButton("SDM_MO_Dir_Folder", SDM_Name_Dir, SDM_Name_Dir),
									verbatimTextOutput("SDM_MO_Dir_Folder", placeholder = TRUE),
									tags$hr(),
									useShinyalert(),  # Set up shinyalert
									actionButton("SDM_MO_SDM_run", label = SDM_Name_models_run)            
								)
							)
						)
					)
				),

				tabPanel(SDM_Name_Model_Out, fluid = TRUE,
					tags$hr(),
					sidebarLayout(
						sidebarPanel(width = 3, Fluid = TRUE,
              shinyDirButton("SDM_AO_Dir_Folder", SDM_Name_Dir, SDM_Name_Dir),
              verbatimTextOutput("SDM_AO_Dir_Folder", placeholder = TRUE),
              tags$hr(),
							uiOutput("SDM_OU_Species"),
							tags$hr(),

							uiOutput("SDM_OU_Projection_model"),
							tags$hr(),

							uiOutput("SDM_OU_Prediction_model"),
							tags$hr(),

							# Input: Checkbox if file has header ----
							radioButtons("SDM_OU_Climate_model", SDM_Name_CD_Models_out,
								choices = c(SDM_Name_CD_Models_out_list),
								selected = SDM_Name_CD_Models_out_selected),

							# Input: Checkbox if file has header ----
							radioButtons("SDM_OU_Climate_scenario", SDM_Name_CD_Scenarios_out,
								choices = c(SDM_Name_CD_Scenarios_out_list),
								selected = SDM_Name_CD_Scenarios_out_selected),

							# Input: Checkbox if file has header ----
							radioButtons("SDM_OU_Project_year", SDM_Name_CD_Year_out,
								choices = c(SDM_Name_CD_Year_out_list),
								selected = SDM_Name_CD_Year_out_selected)
						),

                        # Main panel for displaying outputs ----
						mainPanel(
							tabsetPanel(
								tabPanel(SDM_Name_Model_Out_Validation,
									tags$hr(),
									fluidRow(
										column(6, DT::dataTableOutput("SDM_OU_Validation"))
									),
									tags$hr(),
									fluidRow(
										column(8, plotOutput("SDM_OU_Validation_BoxPlot"))
									)
								),
								tabPanel(SDM_Name_Model_Out_Contribution,
									tags$hr(),
									fluidRow(
										column(6, DT::dataTableOutput("SDM_OU_Contribution"))
									),
									tags$hr(),
									fluidRow(
										column(8,plotOutput("SDM_OU_Contribution_Radarchart"))
									)
								),
								tabPanel(SDM_Name_Model_Out_Probability, 
									tags$head(
										# Include our custom CSS
										includeCSS("styles.css"),
										includeScript("gomap.js")
									),
									tags$hr(),
									leafletOutput("SDM_OU_Probability_map", width = "800", height = "600"),
									tags$hr(),
									column(10, verbatimTextOutput("SDM_OU_PROJ_Summary")),
									column(10, plotOutput("SDM_OU_PROJ_Histogram"))
								),
								tabPanel(SDM_Name_Model_Out_Prediction, 
									tags$head(
										# Include our custom CSS
										includeCSS("styles.css"),
										includeScript("gomap.js")
									),
									tags$hr(),
									leafletOutput("SDM_OU_Predicted_map", width = "800", height = "600"),
									tags$hr(),
									column(10, verbatimTextOutput("SDM_OU_PRED_Summary")),
									column(10, plotOutput("SDM_OU_PRED_Histogram"))
								)
							)
						)
					)
				)
			)
		),  
	
		tabPanel(DM_Name,
		         tabsetPanel(
		           tabPanel(DM_Name_Model, fluid = TRUE,
		             tabsetPanel(
		               tabPanel(DM_Name_Model_SDM,
                      fluidRow(
                      sidebarPanel(width = 2, Fluid = TRUE,
                        shinyDirButton("DM_SDM_Dir_Folder", DM_Name_Dir, DM_Name_Dir),
                        verbatimTextOutput("DM_SDM_Dir_Folder", placeholder = TRUE),
                        tags$hr(),
                        uiOutput("DM_MO_Species")
		                  ),
		                  sidebarPanel(width = 2, Fluid = TRUE,             
                        # Input: Checkbox if file has header ----
                        checkboxGroupInput("DM_MO_Climate_model", DM_Name_CD_Models,
                          choices = c(DM_Name_CD_Models_list),
                          selected = DM_Name_CD_Models_selected),
                        # Input: Checkbox if file has header ----
                        checkboxGroupInput("DM_MO_Climate_scenario", DM_Name_CD_Scenarios,
                          choices = c(DM_Name_CD_Scenarios_list),
                          selected = DM_Name_CD_Scenarios_selected),
                        # Input: Checkbox if file has header ----
                        checkboxGroupInput("DM_MO_Project_year", DM_Name_CD_Year,
                          choices = c(DM_Name_CD_Year_list),
                          selected = DM_Name_CD_Year_selected)
		                  ),
		                  sidebarPanel(width = 3,
                        uiOutput("DM_MO_SDM_model")
		                  )
		                )
		              ),
		              tabPanel(DM_Name_Model_DM,
                    fluidRow(
                      sidebarPanel(width = 3,
#                       uiOutput("DM_MO_DM_envChgSteps"),
                        sliderInput("DM_MO_DM_dispSteps", label = "DM_dispSteps", min = 0, 
                          max = 10, value = 10),
                        checkboxGroupInput("DM_MO_Barrier", DM_Name_DM_MO_Barriers,
                          choices = c(DM_Name_DM_MO_Barriers_list),
                          selected = DM_Name_DM_MO_Barriers_selected
                        ),
                        radioButtons("DM_MO_DM_barrierType", "DM_barrierType",
                          choices = c("strong" = "strong","weak" = "weak"),
                          selected = "weak")
                      ),
                      sidebarPanel(width = 3,
		                                   tags$p("DM_dispKernel:"),
		                                   verbatimTextOutput("DM_MO_DM_dispKernel"),
		                                   sliderInput("DM_MO_DM_dispKernel1", label = "Dispersal Proportion of 1st Pixel", min = 0.01, 
		                                               max = 1.0, step = 0.01, value = 1.0),
		                                   sliderInput("DM_MO_DM_dispKernel2", label = "Dispersal Proportion of 2nd Pixel", min = 0.01, 
		                                               max = 1.0, step = 0.01, value = 0.4),
		                                   sliderInput("DM_MO_DM_dispKernel3", label = "Dispersal Proportion of 3rd Pixel", min = 0.01, 
		                                               max = 1.0, step = 0.01, value = 0.16),
		                                   sliderInput("DM_MO_DM_dispKernel4", label = "Dispersal Proportion of 4th Pixel", min = 0.01, 
		                                               max = 1.0, step = 0.01, value = 0.06),
		                                   sliderInput("DM_MO_DM_dispKernel5", label = "Dispersal Proportion of 5th Pixel", min = 0.01, 
		                                               max = 1.0, step = 0.01, value = 0.03)
                      ),
                      sidebarPanel(width = 3,
                        tags$p("DM_Succession:"),
		                                   sliderInput("DM_MO_DM_iniMatAge", label = "DM_iniMatAge (the initial maturity age)", min = 0, 
		                                               max = 10, value = 5),
		                                   tags$hr(),
		                                   tags$p("DM_propaguleProd:"),
		                                   verbatimTextOutput("DM_MO_DM_propaguleProd"),
		                                   sliderInput("DM_MO_DM_propaguleProd1", label = "Propagule Production Proportion 1st year after iniMatAge", min = 0.01, 
		                                               max = 1.0, step = 0.01, value = 0.01),
		                                   sliderInput("DM_MO_DM_propaguleProd2", label = "Propagule Production Proportion 2nd year after iniMatAge", min = 0.01, 
		                                               max = 1.0, step = 0.01, value = 0.08),
		                                   sliderInput("DM_MO_DM_propaguleProd3", label = "Propagule Production Proportion 3rd year after iniMatAge", min = 0.01, 
		                                               max = 1.0, step = 0.01, value = 0.5),
		                                   sliderInput("DM_MO_DM_propaguleProd4", label = "Propagule Production Proportion 4th year after iniMatAge", min = 0.01, 
		                                               max = 1.0, step = 0.01, value = 0.92)
		                      ),
		                      sidebarPanel(width = 3,
		                                   tags$p("DM_Dispersal Distance:"),
		                                   sliderInput("DM_MO_DM_lddFreq", label = "DM_lddFreq", min = 0.001, 
		                                               max = 1.0, step = 0.001, value = 0.001),
		                                   sliderInput("DM_MO_SDM_lddDist", label = "LDD Distance Range (pixel)", min = 0, 
		                                               max = 10, value = c(6, 10)),
		                                   sliderInput("DM_MO_DM_replicateNb", label = "DM_replicateNb", min = 0, 
		                                               max = 10, value = 1),
		                                   checkboxInput("DM_MO_DM_overWrite", "DM_overWrite", TRUE),
		                                   checkboxInput("DM_MO_DM_testMode", "DM_testMode", FALSE),
		                                   checkboxInput("DM_MO_DM_fullOutput", "DM_fullOutput", FALSE),
		                                   checkboxInput("DM_MO_DM_keepTempFiles", "DM_keepTempFiles", FALSE),
		                                   br(),
		                                   tags$hr(),
#		                                   shinyDirButton("DM_MO_Dir_Folder", DM_Name_Dir, DM_Name_Dir),
#		                                   verbatimTextOutput("DM_MO_Dir_Folder", placeholder = TRUE),
#		                                   tags$hr(),
		                                   useShinyalert(),  # Set up shinyalert
		                                   actionButton("DM_MO_Action_run", label = DM_Name_DM_MO_Action)
		                      )
		                    )
		               )
		             )
		           ),
		           
		           tabPanel(DM_Name_Model_Out, fluid = TRUE,
		                    tags$hr(),
		                    sidebarLayout(
		                      sidebarPanel(width = 3, Fluid = TRUE,
		                                   shinyDirButton("DM_AO_Dir_Folder", DM_Name_Dir, DM_Name_Dir),
		                                   verbatimTextOutput("DM_AO_Dir_Folder", placeholder = TRUE),
		                                   tags$hr(),
		                                   
		                                   uiOutput("DM_OU_Species"),
		                                   tags$hr(),
		                                   
		                                   uiOutput("DM_OU_SDM_model"),
		                                   
		                                   checkboxGroupInput("DM_OU_Dispersal_type", DM_Name_DM_Models,
		                                                      choices = c(DM_Name_DM_Models_list),
		                                                      selected = DM_Name_DM_Models_selected
		                                   ),
		                                   
		                                   # Input: Checkbox if file has header ----
		                                   checkboxGroupInput("DM_OU_Climate_model", DM_Name_CD_Models,
		                                                      choices = c(DM_Name_CD_Models_list),
		                                                      selected = DM_Name_CD_Models_selected
		                                   ),
		                                   
		                                   # Input: Checkbox if file has header ----
		                                   checkboxGroupInput("DM_OU_Climate_scenario", DM_Name_CD_Scenarios,
		                                                      choices = c(DM_Name_CD_Scenarios_list),
		                                                      selected = DM_Name_CD_Scenarios_selected
		                                   ),
		                                   
		                                   # Input: Checkbox if file has header ----
		                                   checkboxGroupInput("DM_OU_Project_year", DM_Name_CD_Year,
		                                                      choices = c(DM_Name_CD_Year_list),
		                                                      selected = DM_Name_CD_Year_selected
		                                   )
		                      ),
		                      
		                      # Main panel for displaying outputs ----
		                      mainPanel(
		                        tabsetPanel(
		                          tabPanel(DM_Name_Out_Plot,
		                                   tags$hr(),
		                                   uiOutput("DM_OU_UI_plot")
		                          )
		                        )
		                      )
		           )
		         )
		      )

		), 
     
		tabPanel(SS_Name,
			tabsetPanel(
				tabPanel(SS_Name_Analysis, fluid = TRUE,
                    tags$hr(),
					fluidRow(
						sidebarPanel(width = 2, Fluid = TRUE,
              shinyDirButton("SS_MO_Dir_Folder", SS_Name_MO_Dir, SS_Name_MO_Dir),
              verbatimTextOutput("SS_MO_Dir_Folder", placeholder = TRUE),
              tags$hr(),
							uiOutput("SS_CA_Species")
						),
						sidebarPanel(width = 3, Fluid = TRUE,             
						    radioButtons("SS_CA_Dispersal_type", SS_Name_DM_Models,
								choices = c(SS_Name_DM_Models_list),
								selected = SS_Name_DM_Models_selected),
	
							# Input: Checkbox if file has header ----
							checkboxGroupInput("SS_CA_Climate_model", SS_Name_CD_Models,
								choices = c(SS_Name_CD_Models_list),
								selected = SS_Name_CD_Models_selected),
                                   
							# Input: Checkbox if file has header ----
							checkboxGroupInput("SS_CA_Climate_scenario", SS_Name_CD_Scenarios,
								choices = c(SS_Name_CD_Scenarios_list),
								selected = SS_Name_CD_Scenarios_selected),
	
							# Input: Checkbox if file has header ----
							checkboxGroupInput("SS_CA_Project_year", SS_Name_CD_Year,
								choices = c(SS_Name_CD_Year_list),
								selected = SS_Name_CD_Year_selected)
						),
						sidebarPanel(width = 4,
							uiOutput("SS_CA_SDM_model"),
							tags$hr(),
							actionButton("SS_CA_Action_change", label = "Analayzing the change of Species Distribution"),
							tags$hr(),
							actionButton("SS_CA_Action_Vindex", label = "Calculating the Climate Vulnerability Index of Species")
						)
					)
				),
	
				tabPanel(SS_Name_Out, fluid = TRUE,
					tags$hr(),
					sidebarLayout(
						sidebarPanel(width = 3, Fluid = TRUE,
              shinyDirButton("SS_AO_Dir_Folder", SS_Name_AO_Dir, SS_Name_AO_Dir),
              verbatimTextOutput("SS_AO_Dir_Folder", placeholder = TRUE),
              tags$hr(),
	
							uiOutput("SS_AO_Species"),
							tags$hr(),
	
							uiOutput("SS_AO_SDM_model"),
	
							radioButtons("SS_AO_Dispersal_type", SS_Name_DM_Models,
								choices = c(SS_Name_DM_Models_list),
								selected = SS_Name_DM_Models_selected),
                                   
							# Input: Checkbox if file has header ----
							checkboxGroupInput("SS_AO_Climate_model", SS_Name_CD_Models,
								choices = c(SS_Name_CD_Models_list),
								selected = SS_Name_CD_Models_selected),
	
							# Input: Checkbox if file has header ----
							checkboxGroupInput("SS_AO_Climate_scenario", SS_Name_CD_Scenarios,
								choices = c(SS_Name_CD_Scenarios_list),
								selected = SS_Name_CD_Scenarios_selected),
                                   
							# Input: Checkbox if file has header ----
							checkboxGroupInput("SS_AO_Project_year", SS_Name_CD_Year,
								choices = c(SS_Name_CD_Year_list),
								selected = SS_Name_CD_Year_selected)
						),
                      
						# Main panel for displaying outputs ----
						mainPanel(
							tabsetPanel(
								tabPanel(SS_Name_Out_ChangePlot,
									tags$hr(),
									uiOutput("SS_AO_UI_plot")
								),
								tabPanel(SS_Name_Out_Pattern, 
									tags$hr(),
									fluidRow(
										column(6, DT::dataTableOutput("SS_AO_IV_Table"))
									),
									fluidRow(
										tags$hr(),
										uiOutput("SS_AO_IV_UI_plot1"),
										tags$hr(),
										column(6, plotOutput("SS_AO_IV_Plot1")),
										column(6, plotOutput("SS_AO_IV_Plot2"))
									),
									fluidRow(
										tags$hr(),
										uiOutput("SS_AO_IV_UI_plot2"),
										tags$hr(),
										column(6, plotOutput("SS_AO_IV_Plot11")),
										column(6, plotOutput("SS_AO_IV_Plot21"))
									)
                ),
								tabPanel(SS_Name_Out_Vulnerabiity, 
									tags$hr(),
									fluidRow(
                    column(6,DT::dataTableOutput("SS_AO_VP_Table"))
									),
									fluidRow(
									  tags$hr(),
									  uiOutput("SS_AO_VP_UI_plot1"),
									  tags$hr(),
									  column(6, plotOutput("SS_AO_VP_Plot1")),
									  column(6, plotOutput("SS_AO_VP_Plot2"))
									),
#									fluidRow(
#									  tags$hr(),
#									  uiOutput("SS_AO_VP_UI_plot2"),
#									  tags$hr(),
#									  column(6, plotOutput("SS_AO_VP_Plot11")),
#									  column(6, plotOutput("SS_AO_VP_Plot21"))
#									),
									fluidRow(
									  tags$hr(),
									  uiOutput("SS_AO_VP_UI_plot3"),
									  tags$hr(),
									  column(6, plotOutput("SS_AO_VP_Plot12")),
									  column(6, plotOutput("SS_AO_VP_Plot22"))
									)
								)
							)
						)
					)
				)
			)
		),      

		tabPanel(IS_Name,
			tabsetPanel(
				tabPanel(IS_Name_Anlayis, fluid = TRUE,
					tags$hr(),
					fluidRow(
						sidebarPanel(width = 3, Fluid = TRUE,
              shinyDirButton("IS_MO_Dir_Folder", IS_Name_MO_Dir, IS_Name_MO_Dir),
              verbatimTextOutput("IS_MO_Dir_Folder", placeholder = TRUE),
              tags$hr(),
							uiOutput("IS_CA_Species")
						),
						sidebarPanel(width = 3, Fluid = TRUE,                                                      
                radioButtons("IS_CA_Dispersal_type", IS_Name_DM_Models,
								choices = c(IS_Name_DM_Models_list),
								selected = IS_Name_DM_Models_selected),
	
							# Input: Checkbox if file has header ----
							checkboxGroupInput("IS_CA_Climate_model", IS_Name_CD_Models,
								choices = c(IS_Name_CD_Models_list),
								selected = IS_Name_CD_Models_selected),
	
							# Input: Checkbox if file has header ----
							checkboxGroupInput("IS_CA_Climate_scenario", IS_Name_CD_Scenarios,
								choices = c(IS_Name_CD_Scenarios_list),
								selected = IS_Name_CD_Scenarios_selected),
	
							# Input: Checkbox if file has header ----
							checkboxGroupInput("IS_CA_Project_year", IS_Name_CD_Year,
								choices = c(IS_Name_CD_Year_list),
								selected = IS_Name_CD_Year_selected)
						),
						sidebarPanel(width = 4,
							uiOutput("IS_CA_SDM_model"),
							tags$hr(),
							shinyDirButton("IS_VA_Dir_Folder", IS_Name_AO_Dir, IS_Name_AO_Dir),
							verbatimTextOutput("IS_VA_Dir_Folder", placeholder = TRUE),
							actionButton("IS_VA_Action_Analysis", label = IS_Name_Action),
							tags$hr(),
							br(),
							checkboxGroupInput("IS_VA_Admin", IS_Name_Admin,
                choices = c(IS_Name_Admin_list),
                selected = IS_Name_Admin_selected),
              actionButton("IS_VA_Action_Admin", label = IS_Name_Action_Admin)
						)
					)
				),
	
				tabPanel(IS_Name_Out, fluid = TRUE,
					tags$hr(),
					sidebarLayout(
						sidebarPanel(width = 3, Fluid = TRUE,
              shinyDirButton("IS_MI_Dir_Folder", IS_Name_MO_Dir, IS_Name_MO_Dir),
              verbatimTextOutput("IS_MI_Dir_Folder", placeholder = TRUE),
              tags$hr(),
							shinyDirButton("IS_AO_Dir_Folder", IS_Name_AO_Dir, IS_Name_AO_Dir),
							verbatimTextOutput("IS_AO_Dir_Folder", placeholder = TRUE),
							tags$hr(),
							#uiOutput("IS_AO_Species"),
							uiOutput("IS_AO_Species"),
							tags$hr(),
							uiOutput("IS_AO_SDM_model"),
	
              radioButtons("IS_AO_Dispersal_type", IS_Name_DM_Models,
								choices = c(IS_Name_DM_Models_list),
								selected = IS_Name_DM_Models_selected),
	
							# Input: Checkbox if file has header ----
              radioButtons("IS_AO_Climate_model", IS_Name_CD_Models,
								choices = c(IS_Name_CD_Models_list),
								selected = IS_Name_CD_Models_selected),
	
							# Input: Checkbox if file has header ----
              radioButtons("IS_AO_Climate_scenario", IS_Name_CD_Scenarios,
								choices = c(IS_Name_CD_Scenarios_list),
								selected = IS_Name_CD_Scenarios_selected),
	
							# Input: Checkbox if file has header ----
              radioButtons("IS_AO_Project_year", IS_Name_CD_Year,
								choices = c(IS_Name_CD_Year_list),
								selected = IS_Name_CD_Year_selected)
						),
	
	# Main panel for displaying outputs ----
						mainPanel(
							tabsetPanel(
							  tabPanel(IS_Name_Out_Species, 
							           tags$head(
							             # Include our custom CSS
							             includeCSS("styles.css"),
							             includeScript("gomap.js")
							           ),
							           tags$hr(),
							           column(6, leafletOutput("IS_AO_SD_Map", width = "800", height = "650")),
							           tags$hr(),
							           column(10, verbatimTextOutput("IS_AO_SD_Summary")),
							           column(10, plotOutput("IS_AO_SD_Histogram"))
							  ),
								tabPanel(IS_Name_Out_SR,
									tabsetPanel(
										tabPanel(IS_Name_Out_Map,
											tags$head(
												# Include our custom CSS
												includeCSS("styles.css"),
												includeScript("gomap.js")
											),
											leafletOutput("IS_AO_SR_Map", width = "800", height = "600")),
										tabPanel(IS_Name_Out_SIDO,
											tabsetPanel(
												tabPanel(IS_Name_Out_Map, 
													tags$head(
														# Include our custom CSS
														includeCSS("styles.css"),
														includeScript("gomap.js")
													),
													leafletOutput("IS_AO_SR_SIDO_Map", width = "800", height = "600")
												),
												tabPanel(IS_Name_Out_Stat, 
													plotOutput("IS_AO_SR_SIDO_Stat")
												)
											)
										),
										tabPanel(IS_Name_Out_SGG,
											tabsetPanel(
												tabPanel(IS_Name_Out_Map, 
													tags$head(
														# Include our custom CSS
														includeCSS("styles.css"),
														includeScript("gomap.js")
													),
													leafletOutput("IS_AO_SR_SGG_Map", width = "800", height = "600")),
												tabPanel(IS_Name_Out_Stat,
												  fluidRow(
												    tags$hr(),
												    uiOutput("IS_AO_SR_SGG_UI"),
												    tags$hr(),
												    plotOutput("IS_AO_SR_SGG_Stat")
												  )
												)
											)
										)
									)
								),
								tabPanel(IS_Name_Out_SI,
                  tabsetPanel(
                    tabPanel(IS_Name_Out_Map,
                      tags$head(
                        # Include our custom CSS
                        includeCSS("styles.css"),
                        includeScript("gomap.js")
                      ),
                      leafletOutput("IS_AO_SI_Map", width = "800", height = "600")),
                    tabPanel(IS_Name_Out_SIDO,
                      tabsetPanel(
                        tabPanel(IS_Name_Out_Map, 
                          tags$head(
                            # Include our custom CSS
                            includeCSS("styles.css"),
                            includeScript("gomap.js")
                          ),
                          leafletOutput("IS_AO_SI_SIDO_Map", width = "800", height = "600")
                        ),
                        tabPanel(IS_Name_Out_Stat, 
                          plotOutput("IS_AO_SI_SIDO_Stat")
                        )
                      )
                    ),
                    tabPanel(IS_Name_Out_SGG,
                      tabsetPanel(
                        tabPanel(IS_Name_Out_Map, 
                          tags$head(
                            # Include our custom CSS
                            includeCSS("styles.css"),
                            includeScript("gomap.js")
                          ),
                          leafletOutput("IS_AO_SI_SGG_Map", width = "800", height = "600")),
                        tabPanel(IS_Name_Out_Stat, 
                          fluidRow(
                            tags$hr(),
                            uiOutput("IS_AO_SI_SGG_UI"),
                            tags$hr(),
                            plotOutput("IS_AO_SI_SGG_Stat")
                          )
                        )
                      )
                    )
                  )
								)
							)
						)
					)
				)
			)    
		),     
     
		tabPanel(VH_Name,
			tabsetPanel(
				tabPanel(VH_Name_Analysis, fluid = TRUE,
					tags$hr(),
					fluidRow(
						sidebarPanel(width = 2, Fluid = TRUE,
						  shinyDirButton("VH_MO_Dir_Folder", VH_Name_MO_Dir, VH_Name_MO_Dir),
						  verbatimTextOutput("VH_MO_Dir_Folder", placeholder = TRUE),
						  tags$hr(),
							uiOutput("VH_CA_Species")
						),
						sidebarPanel(width = 3, Fluid = TRUE,                                                
							checkboxGroupInput("VH_CA_Dispersal_type", VH_Name_DM_Models,
								choices = c(VH_Name_DM_Models_list),
								selected = VH_Name_DM_Models_selected),
	
							# Input: Checkbox if file has header ----
							checkboxGroupInput("VH_CA_Climate_model", VH_Name_CD_Models,
								choices = c(VH_Name_CD_Models_list),
								selected = VH_Name_CD_Models_selected),
                                   
							# Input: Checkbox if file has header ----
							checkboxGroupInput("VH_CA_Climate_scenario", VH_Name_CD_Scenarios,
								choices = c(VH_Name_CD_Scenarios_list),
								selected = VH_Name_CD_Scenarios_selected),
	
							# Input: Checkbox if file has header ----
							checkboxGroupInput("VH_CA_Project_year", VH_Name_CD_Year,
								choices = c(VH_Name_CD_Year_list),
								selected = VH_Name_CD_Year_selected)
						),
						sidebarPanel(width = 4,
						             uiOutput("VH_CA_SDM_model"),
						             tags$hr(),
						             shinyDirButton("VH_VA_Dir_Folder", VH_Name_Dir, VH_Name_Dir),
						             verbatimTextOutput("VH_VA_Dir_Folder", placeholder = TRUE),
						             actionButton("VH_VA_Action_Analysis", label = VH_Name_Action),
						             tags$hr(),
						             br(),
						             checkboxGroupInput("VH_VA_Habitat", VH_Name_Habitat,
						                                choices = c(VH_Name_Habitat_list),
						                                selected = VH_Name_Habitat_selected
						             ),
						             actionButton("VH_VA_Action_Habitat", label = VH_Name_Action_Habitat)
						)
					)
				), 
           
           tabPanel(VH_Name_Out, fluid = TRUE,
                    tags$hr(),
                    sidebarLayout(
                      sidebarPanel(width = 3, Fluid = TRUE,
                                   
                                   shinyDirButton("VH_MI_Dir_Folder", VH_Name_MO_Dir, VH_Name_MO_Dir),
                                   verbatimTextOutput("VH_MI_Dir_Folder", placeholder = TRUE),
                                   tags$hr(),

                                    shinyDirButton("VH_AO_Dir_Folder", VH_Name_Out_Dir, VH_Name_Out_Dir),
                                    verbatimTextOutput("VH_AO_Dir_Folder", placeholder = TRUE),
                                    tags$hr(),  
                                   
                                   uiOutput("VH_AO_Species"),
                                   tags$hr(),
                                   uiOutput("VH_AO_SDM_model"),
                                   
#                                  radioButtons("VH_AO_Dispersal_type", VH_Name_DM_Models,
#                                                      choices = c(VH_Name_DM_Models_list),
#                                                      selected = VH_Name_DM_Models_selected),
                                   
                                   # Input: Checkbox if file has header ----
                                  radioButtons("VH_AO_Climate_model", VH_Name_CD_Models,
                                                      choices = c(VH_Name_CD_Models_list),
                                                      selected = VH_Name_CD_Models_selected),
                                   
                                   # Input: Checkbox if file has header ----
                                  radioButtons("VH_AO_Climate_scenario", VH_Name_CD_Scenarios,
                                                      choices = c(VH_Name_CD_Scenarios_list),
                                                      selected = VH_Name_CD_Scenarios_selected),
                                   
                                   # Input: Checkbox if file has header ----
                                  radioButtons("VH_AO_Project_year", VH_Name_CD_Year,
                                                      choices = c(VH_Name_CD_Year_list),
                                                      selected = VH_Name_CD_Year_selected)
                      ),
                      
                      # Main panel for displaying outputs ----
                      mainPanel(
                        tabsetPanel(
                          tabPanel(VH_Name_Out_SR,
                            tabsetPanel(
                              tabPanel(VH_Name_Out_Map,
                                tags$head(
                                  # Include our custom CSS
                                  includeCSS("styles.css"),
                                  includeScript("gomap.js")
                                ),
                                leafletOutput("VH_AO_SR_Map", width = "800", height = "600")),
                              tabPanel(VH_Name_Out_SIDO,
                                tabsetPanel(
                                  tabPanel(VH_Name_Out_Map, 
                                    tags$head(
                                      # Include our custom CSS
                                      includeCSS("styles.css"),
                                      includeScript("gomap.js")
                                    ),
                                    leafletOutput("VH_AO_SR_SIDO_Map", width = "800", height = "600")
                                  ),
                                  tabPanel(VH_Name_Out_Stat, 
                                    plotOutput("VH_AO_SR_SIDO_Stat")
                                  )
                                )
                              ),
                              tabPanel(VH_Name_Out_SGG,
                                tabsetPanel(
                                  tabPanel(VH_Name_Out_Map, 
                                    tags$head(
                                      # Include our custom CSS
                                      includeCSS("styles.css"),
                                      includeScript("gomap.js")
                                    ),
                                  leafletOutput("VH_AO_SR_SGG_Map", width = "800", height = "600")),
                                tabPanel(VH_Name_Out_Stat, 
                                  fluidRow(
                                    tags$hr(),
                                    uiOutput("VH_AO_SR_SGG_UI"),
                                    tags$hr(),
                                    plotOutput("VH_AO_SR_SGG_Stat")
                                  )
                                )
                              )
                            ),
                            tabPanel(VH_Name_Out_NP,
                                     tabsetPanel(
                                       tabPanel(VH_Name_Out_Map, 
                                                tags$head(
                                                  # Include our custom CSS
                                                  includeCSS("styles.css"),
                                                  includeScript("gomap.js")
                                                ),
                                                leafletOutput("VH_AO_SR_NP_Map", width = "800", height = "600")),
                                       tabPanel(VH_Name_Out_Stat, 
                                                plotOutput("VH_AO_SR_NP_Stat")
                                       )
                                     )
                            ),
                            tabPanel(VH_Name_Out_BR,
                                     tabsetPanel(
                                       tabPanel(VH_Name_Out_Map, 
                                                tags$head(
                                                  # Include our custom CSS
                                                  includeCSS("styles.css"),
                                                  includeScript("gomap.js")
                                                ),
                                                leafletOutput("VH_AO_SR_BR_Map", width = "800", height = "600")),
                                       tabPanel(VH_Name_Out_Stat, 
                                                plotOutput("VH_AO_SR_BR_Stat")
                                       )
                                     )
                            ),
                            tabPanel(VH_Name_Out_DMZ,
                                     tabsetPanel(
                                       tabPanel(VH_Name_Out_Map, 
                                                tags$head(
                                                  # Include our custom CSS
                                                  includeCSS("styles.css"),
                                                  includeScript("gomap.js")
                                                ),
                                                leafletOutput("VH_AO_SR_DMZ_Map", width = "800", height = "600")),
                                       tabPanel(VH_Name_Out_Stat, 
                                                plotOutput("VH_AO_SR_DMZ_Stat")
                                       )
                                     )
                            )
                          )
                        ),
                        
                        tabPanel(VH_Name_Out_SL,
                                 tabsetPanel(
                                   tabPanel(VH_Name_Out_Map,
                                            tags$head(
                                              # Include our custom CSS
                                              includeCSS("styles.css"),
                                              includeScript("gomap.js")
                                            ),
                                            leafletOutput("VH_AO_SL_Map", width = "800", height = "600")),
                                   tabPanel(VH_Name_Out_SIDO,
                                            tabsetPanel(
                                              tabPanel(VH_Name_Out_Map, 
                                                       tags$head(
                                                         # Include our custom CSS
                                                         includeCSS("styles.css"),
                                                         includeScript("gomap.js")
                                                       ),
                                                       leafletOutput("VH_AO_SL_SIDO_Map", width = "800", height = "600")
                                              ),
                                              tabPanel(VH_Name_Out_Stat, 
                                                       plotOutput("VH_AO_SL_SIDO_Stat")
                                              )
                                            )
                                   ),
                                   tabPanel(VH_Name_Out_SGG,
                                            tabsetPanel(
                                              tabPanel(VH_Name_Out_Map, 
                                                       tags$head(
                                                         # Include our custom CSS
                                                         includeCSS("styles.css"),
                                                         includeScript("gomap.js")
                                                       ),
                                                       leafletOutput("VH_AO_SL_SGG_Map", width = "800", height = "600")),
                                              tabPanel(VH_Name_Out_Stat, 
                                                       fluidRow(
                                                         tags$hr(),
                                                         uiOutput("VH_AO_SL_SGG_UI"),
                                                         tags$hr(),
                                                         plotOutput("VH_AO_SL_SGG_Stat")
                                                       )
                                              )
                                            )
                                   ),
                                   tabPanel(VH_Name_Out_NP,
                                            tabsetPanel(
                                              tabPanel(VH_Name_Out_Map, 
                                                       tags$head(
                                                         # Include our custom CSS
                                                         includeCSS("styles.css"),
                                                         includeScript("gomap.js")
                                                       ),
                                                       leafletOutput("VH_AO_SL_NP_Map", width = "800", height = "600")),
                                              tabPanel(VH_Name_Out_Stat, 
                                                       plotOutput("VH_AO_SL_NP_Stat")
                                              )
                                            )
                                   ),
                                   tabPanel(VH_Name_Out_BR,
                                            tabsetPanel(
                                              tabPanel(VH_Name_Out_Map, 
                                                       tags$head(
                                                         # Include our custom CSS
                                                         includeCSS("styles.css"),
                                                         includeScript("gomap.js")
                                                       ),
                                                       leafletOutput("VH_AO_SL_BR_Map", width = "800", height = "600")),
                                              tabPanel(VH_Name_Out_Stat, 
                                                       plotOutput("VH_AO_SL_BR_Stat")
                                              )
                                            )
                                   ),
                                   tabPanel(VH_Name_Out_DMZ,
                                            tabsetPanel(
                                              tabPanel(VH_Name_Out_Map, 
                                                       tags$head(
                                                         # Include our custom CSS
                                                         includeCSS("styles.css"),
                                                         includeScript("gomap.js")
                                                       ),
                                                       leafletOutput("VH_AO_SL_DMZ_Map", width = "800", height = "600")),
                                              tabPanel(VH_Name_Out_Stat, 
                                                       plotOutput("VH_AO_SL_DMZ_Stat")
                                              )
                                            )
                                   )
                                 )
                        ),
                        
                        tabPanel(VH_Name_Out_SS,
                                 tabsetPanel(
                                   tabPanel(VH_Name_Out_Map,
                                            tags$head(
                                              # Include our custom CSS
                                              includeCSS("styles.css"),
                                              includeScript("gomap.js")
                                            ),
                                            leafletOutput("VH_AO_SS_Map", width = "800", height = "600")),
                                   tabPanel(VH_Name_Out_SIDO,
                                            tabsetPanel(
                                              tabPanel(VH_Name_Out_Map, 
                                                       tags$head(
                                                         # Include our custom CSS
                                                         includeCSS("styles.css"),
                                                         includeScript("gomap.js")
                                                       ),
                                                       leafletOutput("VH_AO_SS_SIDO_Map", width = "800", height = "600")
                                              ),
                                              tabPanel(VH_Name_Out_Stat, 
                                                       fluidRow(
                                                         tags$hr(),
                                                         uiOutput("VH_AO_SS_SGG_UI"),
                                                         tags$hr(),
                                                         plotOutput("VH_AO_SS_SIDO_Stat")
                                                       )
                                              )
                                            )
                                   ),
                                   tabPanel(VH_Name_Out_SGG,
                                            tabsetPanel(
                                              tabPanel(VH_Name_Out_Map, 
                                                       tags$head(
                                                         # Include our custom CSS
                                                         includeCSS("styles.css"),
                                                         includeScript("gomap.js")
                                                       ),
                                                       leafletOutput("VH_AO_SS_SGG_Map", width = "800", height = "600")),
                                              tabPanel(VH_Name_Out_Stat, 
                                                       plotOutput("VH_AO_SS_SGG_Stat")
                                              )
                                            )
                                   ),
                                   tabPanel(VH_Name_Out_NP,
                                            tabsetPanel(
                                              tabPanel(VH_Name_Out_Map, 
                                                       tags$head(
                                                         # Include our custom CSS
                                                         includeCSS("styles.css"),
                                                         includeScript("gomap.js")
                                                       ),
                                                       leafletOutput("VH_AO_SS_NP_Map", width = "800", height = "600")),
                                              tabPanel(VH_Name_Out_Stat, 
                                                       plotOutput("VH_AO_SS_NP_Stat")
                                              )
                                            )
                                   ),
                                   tabPanel(VH_Name_Out_BR,
                                            tabsetPanel(
                                              tabPanel(VH_Name_Out_Map, 
                                                       tags$head(
                                                         # Include our custom CSS
                                                         includeCSS("styles.css"),
                                                         includeScript("gomap.js")
                                                       ),
                                                       leafletOutput("VH_AO_SS_BR_Map", width = "800", height = "600")),
                                              tabPanel(VH_Name_Out_Stat, 
                                                       plotOutput("VH_AO_SS_BR_Stat")
                                              )
                                            )
                                   ),
                                   tabPanel(VH_Name_Out_DMZ,
                                            tabsetPanel(
                                              tabPanel(VH_Name_Out_Map, 
                                                       tags$head(
                                                         # Include our custom CSS
                                                         includeCSS("styles.css"),
                                                         includeScript("gomap.js")
                                                       ),
                                                       leafletOutput("VH_AO_SS_DMZ_Map", width = "800", height = "600")),
                                              tabPanel(VH_Name_Out_Stat, 
                                                       plotOutput("VH_AO_SS_DMZ_Stat")
                                              )
                                            )
                                   )
                                 )
                        ),
                        
                        tabPanel(VH_Name_Out_SI,
                                 tabsetPanel(
                                   tabPanel(VH_Name_Out_Map,
                                            tags$head(
                                              # Include our custom CSS
                                              includeCSS("styles.css"),
                                              includeScript("gomap.js")
                                            ),
                                            leafletOutput("VH_AO_SI_Map", width = "800", height = "600")),
                                   tabPanel(VH_Name_Out_SIDO,
                                            tabsetPanel(
                                              tabPanel(VH_Name_Out_Map, 
                                                       tags$head(
                                                         # Include our custom CSS
                                                         includeCSS("styles.css"),
                                                         includeScript("gomap.js")
                                                       ),
                                                       leafletOutput("VH_AO_SI_SIDO_Map", width = "800", height = "600")
                                              ),
                                              tabPanel(VH_Name_Out_Stat, 
                                                         plotOutput("VH_AO_SI_SIDO_Stat")
                                              )
                                            )
                                   ),
                                   tabPanel(VH_Name_Out_SGG,
                                            tabsetPanel(
                                              tabPanel(VH_Name_Out_Map, 
                                                       tags$head(
                                                         # Include our custom CSS
                                                         includeCSS("styles.css"),
                                                         includeScript("gomap.js")
                                                       ),
                                                       leafletOutput("VH_AO_SI_SGG_Map", width = "800", height = "600")),
                                              tabPanel(VH_Name_Out_Stat, 
                                                       fluidRow(
                                                         tags$hr(),
                                                         uiOutput("VH_AO_SI_SGG_UI"),
                                                         tags$hr(),
                                                         plotOutput("VH_AO_SI_SGG_Stat")
                                                       )
                                              )
                                            )
                                   ),
                                   tabPanel(VH_Name_Out_NP,
                                            tabsetPanel(
                                              tabPanel(VH_Name_Out_Map, 
                                                       tags$head(
                                                         # Include our custom CSS
                                                         includeCSS("styles.css"),
                                                         includeScript("gomap.js")
                                                       ),
                                                       leafletOutput("VH_AO_SI_NP_Map", width = "800", height = "600")),
                                              tabPanel(VH_Name_Out_Stat, 
                                                       plotOutput("VH_AO_SI_NP_Stat")
                                              )
                                            )
                                   ),
                                   tabPanel(VH_Name_Out_BR,
                                            tabsetPanel(
                                              tabPanel(VH_Name_Out_Map, 
                                                       tags$head(
                                                         # Include our custom CSS
                                                         includeCSS("styles.css"),
                                                         includeScript("gomap.js")
                                                       ),
                                                       leafletOutput("VH_AO_SI_BR_Map", width = "800", height = "600")),
                                              tabPanel(VH_Name_Out_Stat, 
                                                       plotOutput("VH_AO_SI_BR_Stat")
                                              )
                                            )
                                   ),
                                   tabPanel(VH_Name_Out_DMZ,
                                            tabsetPanel(
                                              tabPanel(VH_Name_Out_Map, 
                                                       tags$head(
                                                         # Include our custom CSS
                                                         includeCSS("styles.css"),
                                                         includeScript("gomap.js")
                                                       ),
                                                       leafletOutput("VH_AO_SI_DMZ_Map", width = "800", height = "600")),
                                              tabPanel(VH_Name_Out_Stat, 
                                                       plotOutput("VH_AO_SI_DMZ_Stat")
                                              )
                                            )
                                   )
                                 )
                        ),
                        
                        tabPanel(VH_Name_Out_VI1,
                                 tabsetPanel(
                                   tabPanel(VH_Name_Out_Map,
                                            tags$head(
                                              # Include our custom CSS
                                              includeCSS("styles.css"),
                                              includeScript("gomap.js")
                                            ),
                                            leafletOutput("VH_AO_VI1_Map", width = "800", height = "600")),
                                   tabPanel(VH_Name_Out_SIDO,
                                            tabsetPanel(
                                              tabPanel(VH_Name_Out_Map, 
                                                       tags$head(
                                                         # Include our custom CSS
                                                         includeCSS("styles.css"),
                                                         includeScript("gomap.js")
                                                       ),
                                                       leafletOutput("VH_AO_VI1_SIDO_Map", width = "800", height = "600")
                                              ),
                                              tabPanel(VH_Name_Out_Stat,
                                                         plotOutput("VH_AO_VI1_SIDO_Stat")
                                              )
                                            )
                                   ),
                                   tabPanel(VH_Name_Out_SGG,
                                            tabsetPanel(
                                              tabPanel(VH_Name_Out_Map, 
                                                       tags$head(
                                                         # Include our custom CSS
                                                         includeCSS("styles.css"),
                                                         includeScript("gomap.js")
                                                       ),
                                                       leafletOutput("VH_AO_VI1_SGG_Map", width = "800", height = "600")),
                                              tabPanel(VH_Name_Out_Stat, 
                                                       fluidRow(
                                                         tags$hr(),
                                                         uiOutput("VH_AO_VI1_SGG_UI"),
                                                         tags$hr(),
                                                         plotOutput("VH_AO_VI1_SGG_Stat")
                                                       )
                                              )
                                            )
                                   ),
                                   tabPanel(VH_Name_Out_NP,
                                            tabsetPanel(
                                              tabPanel(VH_Name_Out_Map, 
                                                       tags$head(
                                                         # Include our custom CSS
                                                         includeCSS("styles.css"),
                                                         includeScript("gomap.js")
                                                       ),
                                                       leafletOutput("VH_AO_VI1_NP_Map", width = "800", height = "600")),
                                              tabPanel(VH_Name_Out_Stat, 
                                                       plotOutput("VH_AO_VI1_NP_Stat")
                                              )
                                            )
                                   ),
                                   tabPanel(VH_Name_Out_BR,
                                            tabsetPanel(
                                              tabPanel(VH_Name_Out_Map, 
                                                       tags$head(
                                                         # Include our custom CSS
                                                         includeCSS("styles.css"),
                                                         includeScript("gomap.js")
                                                       ),
                                                       leafletOutput("VH_AO_VI1_BR_Map", width = "800", height = "600")),
                                              tabPanel(VH_Name_Out_Stat, 
                                                       plotOutput("VH_AO_VI1_BR_Stat")
                                              )
                                            )
                                   ),
                                   tabPanel(VH_Name_Out_DMZ,
                                            tabsetPanel(
                                              tabPanel(VH_Name_Out_Map, 
                                                       tags$head(
                                                         # Include our custom CSS
                                                         includeCSS("styles.css"),
                                                         includeScript("gomap.js")
                                                       ),
                                                       leafletOutput("VH_AO_VI1_DMZ_Map", width = "800", height = "600")),
                                              tabPanel(VH_Name_Out_Stat, 
                                                       plotOutput("VH_AO_VI1_DMZ_Stat")
                                              )
                                            )
                                   )
                                 )
                        ),
                        
                        tabPanel(VH_Name_Out_VI2,
                                 tabsetPanel(
                                   tabPanel(VH_Name_Out_Map,
                                            tags$head(
                                              # Include our custom CSS
                                              includeCSS("styles.css"),
                                              includeScript("gomap.js")
                                            ),
                                            leafletOutput("VH_AO_VI2_Map", width = "800", height = "600")),
                                   tabPanel(VH_Name_Out_SIDO,
                                            tabsetPanel(
                                              tabPanel(VH_Name_Out_Map, 
                                                       tags$head(
                                                         # Include our custom CSS
                                                         includeCSS("styles.css"),
                                                         includeScript("gomap.js")
                                                       ),
                                                       leafletOutput("VH_AO_VI2_SIDO_Map", width = "800", height = "600")
                                              ),
                                              tabPanel(VH_Name_Out_Stat, 
                                                       plotOutput("VH_AO_VI2_SIDO_Stat")
                                              )
                                            )
                                   ),
                                   tabPanel(VH_Name_Out_SGG,
                                            tabsetPanel(
                                              tabPanel(VH_Name_Out_Map, 
                                                       tags$head(
                                                         # Include our custom CSS
                                                         includeCSS("styles.css"),
                                                         includeScript("gomap.js")
                                                       ),
                                                       leafletOutput("VH_AO_VI2_SGG_Map", width = "800", height = "600")),
                                              tabPanel(VH_Name_Out_Stat, 
                                                       fluidRow(
                                                         tags$hr(),
                                                         uiOutput("VH_AO_VI2_SGG_UI"),
                                                         tags$hr(),
                                                         plotOutput("VH_AO_VI2_SGG_Stat")
                                                       )
                                              )
                                            )
                                   ),
                                   tabPanel(VH_Name_Out_NP,
                                            tabsetPanel(
                                              tabPanel(VH_Name_Out_Map, 
                                                       tags$head(
                                                         # Include our custom CSS
                                                         includeCSS("styles.css"),
                                                         includeScript("gomap.js")
                                                       ),
                                                       leafletOutput("VH_AO_VI2_NP_Map", width = "800", height = "600")),
                                              tabPanel(VH_Name_Out_Stat, 
                                                       plotOutput("VH_AO_VI2_NP_Stat")
                                              )
                                            )
                                   ),
                                   tabPanel(VH_Name_Out_BR,
                                            tabsetPanel(
                                              tabPanel(VH_Name_Out_Map, 
                                                       tags$head(
                                                         # Include our custom CSS
                                                         includeCSS("styles.css"),
                                                         includeScript("gomap.js")
                                                       ),
                                                       leafletOutput("VH_AO_VI2_BR_Map", width = "800", height = "600")),
                                              tabPanel(VH_Name_Out_Stat, 
                                                       plotOutput("VH_AO_VI2_BR_Stat")
                                              )
                                            )
                                   ),
                                   tabPanel(VH_Name_Out_DMZ,
                                            tabsetPanel(
                                              tabPanel(VH_Name_Out_Map, 
                                                       tags$head(
                                                         # Include our custom CSS
                                                         includeCSS("styles.css"),
                                                         includeScript("gomap.js")
                                                       ),
                                                       leafletOutput("VH_AO_VI2_DMZ_Map", width = "800", height = "600")),
                                              tabPanel(VH_Name_Out_Stat, 
                                                       plotOutput("VH_AO_VI2_DMZ_Stat")
                                              )
                                            )
                                   )
                                 )
                        ),
                        tabPanel(VH_Name_Out_VI3,
                                 tabsetPanel(
                                   tabPanel(VH_Name_Out_Map,
                                            tags$head(
                                              # Include our custom CSS
                                              includeCSS("styles.css"),
                                              includeScript("gomap.js")
                                            ),
                                            leafletOutput("VH_AO_VI3_Map", width = "800", height = "600")),
                                   tabPanel(VH_Name_Out_SIDO,
                                            tabsetPanel(
                                              tabPanel(VH_Name_Out_Map, 
                                                       tags$head(
                                                         # Include our custom CSS
                                                         includeCSS("styles.css"),
                                                         includeScript("gomap.js")
                                                       ),
                                                       leafletOutput("VH_AO_VI3_SIDO_Map", width = "800", height = "600")
                                              ),
                                              tabPanel(VH_Name_Out_Stat, 
                                                       plotOutput("VH_AO_VI3_SIDO_Stat")
                                              )
                                            )
                                   ),
                                   tabPanel(VH_Name_Out_SGG,
                                            tabsetPanel(
                                              tabPanel(VH_Name_Out_Map, 
                                                       tags$head(
                                                         # Include our custom CSS
                                                         includeCSS("styles.css"),
                                                         includeScript("gomap.js")
                                                       ),
                                                       leafletOutput("VH_AO_VI3_SGG_Map", width = "800", height = "600")),
                                              tabPanel(VH_Name_Out_Stat, 
                                                       fluidRow(
                                                         tags$hr(),
                                                         uiOutput("VH_AO_VI3_SGG_UI"),
                                                         tags$hr(),
                                                         plotOutput("VH_AO_VI3_SGG_Stat")
                                                       )
                                              )
                                            )
                                   ),
                                   tabPanel(VH_Name_Out_NP,
                                            tabsetPanel(
                                              tabPanel(VH_Name_Out_Map, 
                                                       tags$head(
                                                         # Include our custom CSS
                                                         includeCSS("styles.css"),
                                                         includeScript("gomap.js")
                                                       ),
                                                       leafletOutput("VH_AO_VI3_NP_Map", width = "800", height = "600")),
                                              tabPanel(VH_Name_Out_Stat, 
                                                       plotOutput("VH_AO_VI3_NP_Stat")
                                              )
                                            )
                                   ),
                                   tabPanel(VH_Name_Out_BR,
                                            tabsetPanel(
                                              tabPanel(VH_Name_Out_Map, 
                                                       tags$head(
                                                         # Include our custom CSS
                                                         includeCSS("styles.css"),
                                                         includeScript("gomap.js")
                                                       ),
                                                       leafletOutput("VH_AO_VI3_BR_Map", width = "800", height = "600")),
                                              tabPanel(VH_Name_Out_Stat, 
                                                       plotOutput("VH_AO_VI3_BR_Stat")
                                              )
                                            )
                                   ),
                                   tabPanel(VH_Name_Out_DMZ,
                                            tabsetPanel(
                                              tabPanel(VH_Name_Out_Map, 
                                                       tags$head(
                                                         # Include our custom CSS
                                                         includeCSS("styles.css"),
                                                         includeScript("gomap.js")
                                                       ),
                                                       leafletOutput("VH_AO_VI3_DMZ_Map", width = "800", height = "600")),
                                              tabPanel(VH_Name_Out_Stat, 
                                                       plotOutput("VH_AO_VI3_DMZ_Stat")
                                              )
                                            )
                                   )
                                 )
                        )
							)
						)
          )
				)
			)
		),          
	
		tabPanel(HELP_Name, fluid = TRUE,
			tags$hr(),
			sidebarPanel(width = 5,
        helpText(h5(HELP_Name_Content))
			)
		)
	)
	)
)
)
)
