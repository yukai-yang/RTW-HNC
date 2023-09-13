# data manipulation

actions = c("UniEdu", "Spouse", "House", "WhiteC", "PinkC", "Oropharynx", "Oral", "Larynx",
            "LateStage", "MulModality","Smoking")

tab = data.frame()

tab = rbind(tab, data.frame(Variables = "IRTW", Values = "",
                            Descriptions = "returned to work at some point within 12 months after the medical treatment"))

tab = rbind(tab, data.frame(Variables = "IRTW-P1P2", Values = "",
                            Descriptions = "returned to work within 3 months after the medical treatment and kept working up to 12 months or until retirement"))
#tab = rbind(tab, data.frame(Variables = "IRTW3m", Values = "= 0",
#                            Descriptions = "did not return to work within 3 months after the medical treatment"))

#tab = rbind(tab, data.frame(Variables = "IRTW3m'", Values = "",
#                            Descriptions = "returned to work within 3 months after the medical treatment but could not keep working afterwards"))
#tab = rbind(tab, data.frame(Variables = "IRTW3m'", Values = "= 0",
#                            Descriptions = "did not return to work within 3 months after the medical treatment"))

tab = rbind(tab, data.frame(Variables = "IRTW-P2", Values = "",
                            Descriptions = "returned to work at some point in between 3 and 12 months after the medical treatment"))
#tab = rbind(tab, data.frame(Variables = "IRTW12m", Values = "= 0",
#                            Descriptions = "did not return to work within 12 months after the medical treatment"))

tab = rbind(tab, data.frame(Variables = "UniEdu", Values = "= 0",
                            Descriptions = "have not entered college or university"))
tab = rbind(tab, data.frame(Variables = "UniEdu", Values = "= 1",
                            Descriptions = "have entered colledge or university"))

tab = rbind(tab, data.frame(Variables = "Spouse", Values = "= 0",
                            Descriptions = "do not live with the spouse or partner, or do not have spouse or partner"))
tab = rbind(tab, data.frame(Variables = "Spouse", Values = "= 1",
                            Descriptions = "live with the spouse or partner"))


tab = rbind(tab, data.frame(Variables = "House", Values = "= 0",
                            Descriptions = "do not live in a house"))
tab = rbind(tab, data.frame(Variables = "House", Values = "= 1",
                            Descriptions = "live in a house"))

tab = rbind(tab, data.frame(Variables = "WhiteC", Values = "= 0",
                            Descriptions = "have not been working as white collar"))
tab = rbind(tab, data.frame(Variables = "WhiteC", Values = "= 1",
                            Descriptions = "have been workiing as white collar"))

tab = rbind(tab, data.frame(Variables = "PinkC", Values = "= 0",
                            Descriptions = "have not been working as pink collar, health care or equivalent"))
tab = rbind(tab, data.frame(Variables = "PinkC", Values = "= 1",
                            Descriptions = "have been workiing as pink collar, health care or equivalent"))

tab = rbind(tab, data.frame(Variables = "Oropharynx", Values = "= 0",
                            Descriptions = "not oropharynx cancer"))
tab = rbind(tab, data.frame(Variables = "Oropharynx", Values = "= 1",
                            Descriptions = "oropharynx cancer"))

tab = rbind(tab, data.frame(Variables = "Oral", Values = "= 0",
                            Descriptions = "not oral cancer"))
tab = rbind(tab, data.frame(Variables = "Oral", Values = "= 1",
                            Descriptions = "oral cancer"))

tab = rbind(tab, data.frame(Variables = "Larynx", Values = "= 0",
                            Descriptions = "not larynx cancer"))
tab = rbind(tab, data.frame(Variables = "Larynx", Values = "= 1",
                            Descriptions = "larynx cancer"))

tab = rbind(tab, data.frame(Variables = "LateStage", Values = "= 0",
                            Descriptions = "cancer stage I or II"))
tab = rbind(tab, data.frame(Variables = "LateStage", Values = "= 1",
                            Descriptions = "cancer stage III or IV"))

tab = rbind(tab, data.frame(Variables = "MulModality", Values = "= 0",
                            Descriptions = "single treatment modality"))
tab = rbind(tab, data.frame(Variables = "MulModality", Values = "= 1",
                            Descriptions = "multiple treatment modality"))

tab = rbind(tab, data.frame(Variables = "Smoking", Values = "= 0",
                            Descriptions = "have never smoked"))
tab = rbind(tab, data.frame(Variables = "Smoking", Values = "= 1",
                            Descriptions = "smoking or ever smoked"))