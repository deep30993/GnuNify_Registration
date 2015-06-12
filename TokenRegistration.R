library("gWidgets2")
library("gWidgetstcltk")
library("gWidgets2tcltk")
library("XLConnect")
rwindow <- gwindow(title="Register Window")
main1 <- gframe("Token Window", container = rwindow)
tbl1<-glayout(container=main1) 
tbl1[1,1]<-"Token No: "
tbl1[1,2]<-(txt_Token <- gedit(container=tbl1))
tbl1[1,3]<-(btn_check <- gbutton(container=tbl1,text="Check"))

main2 <- gframe("Details Window", container = rwindow)
tbl2<-glayout(container=main2) 
tbl2[1,1]<-"Name: "
tbl2[1,2]<-(txt_name <- gedit(container=tbl2))
tbl2[2,1]<-"Email: "
tbl2[2,2]<-(txt_email <- gedit(container=tbl2))
tbl2[3,1]<-"Gender: "
tbl2[3,2]<-(radio_gender <- gradio(c("Male","Female"),container=tbl2))
tbl2[4,1]<-"City: "
tbl2[4,2]<-(txt_city <- gedit(container=tbl2))
tbl2[5,1]<-"State: "
tbl2[5,2]<-(txt_state <- gedit(container=tbl2))
tbl2[6,1]<-"Country: "
tbl2[6,2]<-(txt_country <- gedit(container=tbl2))
tbl2[7,1]<-"Occupation: "
tbl2[7,2]<-(txt_occ <- gedit(container=tbl2))
tbl2[8,1]<-"College/Company Name: "
tbl2[8,2]<-(txt_clgcomName <- gedit(container=tbl2))
tbl2[9,1]<-"Tracks: "
tbl2[10,1]<-(event1 <- gcheckbox(text="Big Data",container=tbl2))
tbl2[10,2]<-(event2 <- gcheckbox(text="Drupal",container=tbl2))
tbl2[10,3]<-(event3 <- gcheckbox(text="Embedded Technologies",container=tbl2))
tbl2[11,1]<-(event4 <- gcheckbox(text="FOSS General",container=tbl2))
tbl2[11,2]<-(event5 <- gcheckbox(text="Mobile technologies",container=tbl2))
tbl2[11,3]<-(event6 <- gcheckbox(text="Mozilla",container=tbl2))
tbl2[12,1]<-(event7 <- gcheckbox(text="OpenStack Miniconf",container=tbl2))
tbl2[12,2]<-(event8 <- gcheckbox(text="Python",container=tbl2))
tbl2[12,3]<-(event9 <- gcheckbox(text="Systemadmin & Security",container=tbl2))
tbl2[13,1]<-(event10 <- gcheckbox(text="Web technologies",container=tbl2))
tbl2[13,2]<-(event11 <- gcheckbox(text="Community Events",container=tbl2))
tbl2[13,3]<-(event12 <- gcheckbox(text="Other",container=tbl2))
tbl2[14,1]<-(btn_Submit <- gbutton(container=tbl2,text="Submit"))
tbl2[14,2]<-(btn_clear <- gbutton(container=tbl2,text="Clear"))


do_clear <- function(obj){
  svalue(txt_Token)<-""
  svalue(txt_name)<-""
  svalue(txt_email)<-""
  svalue(radio_gender)<-"Male"
  svalue(txt_city)<-""
  svalue(txt_state)<-""
  svalue(txt_country)<-""
  svalue(txt_occ)<-""
  svalue(txt_clgcomName)<-""
  svalue(event1)<-FALSE
  svalue(event2)<-FALSE
  svalue(event3)<-FALSE
  svalue(event4)<-FALSE
  svalue(event5)<-FALSE
  svalue(event6)<-FALSE
  svalue(event7)<-FALSE
  svalue(event8)<-FALSE
  svalue(event9)<-FALSE
  svalue(event10)<-FALSE
  svalue(event11)<-FALSE
  svalue(event12)<-FALSE
} 
do_check <- function(obj){
	wb = loadWorkbook("gnunifyFinal.xlsx")
	df = readWorksheet(wb, sheet = "new1")
	uid<-svalue(txt_Token)
	uid<-as.character(uid)
	udata<-df[match(uid, df$ID),]
	svalue(txt_name)<-as.character(udata[2])
	svalue(txt_email)<-as.character(udata[3])
	if(as.character(udata[4])=="Male")
	{
		svalue(radio_gender)<-"Male"
	}
	else
	{
		svalue(radio_gender)<-"Female"
	}
	svalue(txt_city)<-as.character(udata[5])
	svalue(txt_state)<-as.character(udata[6])
	svalue(txt_country)<-as.character(udata[7])
	svalue(txt_occ)<-as.character(udata[8])
	svalue(txt_clgcomName)<-as.character(udata[9])
	if(as.character(udata[10])=="Yes")
	{
		svalue(event1)<-TRUE
	}
	if(as.character(udata[11])=="Yes")
	{
		svalue(event2)<-TRUE
	}
	if(as.character(udata[12])=="Yes")
	{
		svalue(event3)<-TRUE
	}
	if(as.character(udata[13])=="Yes")
	{
		svalue(event4)<-TRUE
	}
	if(as.character(udata[14])=="Yes")
	{
		svalue(event5)<-TRUE
	}
	if(as.character(udata[15])=="Yes")
	{
		svalue(event6)<-TRUE
	}
	if(as.character(udata[16])=="Yes")
	{
		svalue(event7)<-TRUE
	}
	if(as.character(udata[17])=="Yes")
	{
		svalue(event8)<-TRUE
	}
	if(as.character(udata[18])=="Yes")
	{
		svalue(event9)<-TRUE
	}
	if(as.character(udata[19])=="Yes")
	{
		svalue(event10)<-TRUE
	}
	if(as.character(udata[20])=="Yes")
	{
		svalue(event11)<-TRUE
	}
	if(as.character(udata[21])=="Yes")
	{
		svalue(event12)<-TRUE
	}
	
}
do_submit <- function(obj){
	wb = loadWorkbook("attendgnunifyFinal.xlsx")
	uname<-svalue(txt_name)
	uemail<-svalue(txt_email)
	ugender<-svalue(radio_gender)
	ucity<-svalue(txt_city)
	ustate<-svalue(txt_state)
	ucountry<-svalue(txt_country)
	uocc<-svalue(txt_occ)
	uclgname<-svalue(txt_clgcomName)
	uevent1<-svalue(event1)
	if(uevent1=="FALSE")
	{
		u1event1<-"No"
	}
	else
	{
		u1event1<-"Yes"
	}
	uevent2<-svalue(event2)
	if(uevent2=="FALSE")
	{
		u1event2<-"No"
	}
	else
	{
		u1event2<-"Yes"
	}
	uevent3<-svalue(event3)
	if(uevent3=="FALSE")
	{
		u1event3<-"No"
	}
	else
	{
		u1event3<-"Yes"
	}
	uevent4<-svalue(event4)
	if(uevent4=="FALSE")
	{
		u1event4<-"No"
	}
	else
	{
		u1event4<-"Yes"
	}
	uevent5<-svalue(event5)
	if(uevent5=="FALSE")
	{
		u1event5<-"No"
	}
	else
	{
		u1event5<-"Yes"
	}
	uevent6<-svalue(event6)
	if(uevent6=="FALSE")
	{
		u1event6<-"No"
	}
	else
	{
		u1event6<-"Yes"
	}
	uevent7<-svalue(event7)
	if(uevent7=="FALSE")
	{
		u1event7<-"No"
	}
	else
	{
		u1event7<-"Yes"
	}
	uevent8<-svalue(event8)
	if(uevent8=="FALSE")
	{
		u1event8<-"No"
	}
	else
	{
		u1event8<-"Yes"
	}
	uevent9<-svalue(event9)
	if(uevent9=="FALSE")
	{
		u1event9<-"No"
	}
	else
	{
		u1event9<-"Yes"
	}
	uevent10<-svalue(event10)
	if(uevent10=="FALSE")
	{
		u1event10<-"No"
	}
	else
	{
		u1event10<-"Yes"
	}
	uevent11<-svalue(event11)
	if(uevent11=="FALSE")
	{
		u1event11<-"No"
	}
	else
	{
		u1event11<-"Yes"
	}
	uevent12<-svalue(event12)
	if(uevent12=="FALSE")
	{
		u1event12<-"No"
	}
	else
	{
		u1event12<-"Yes"
	}
	a<-getLastRow(wb,"new1")
	a<-a+1
	udata<-data.frame(uid,uname,uemail,ugender,ucity,ustate,ucountry,uocc,uclgname,u1event1,u1event2,u1event3,u1event4,u1event5,u1event6,u1event7,u1event8,u1event9,u1event10,u1event11,u1event12)
	writeWorksheet(wb, udata, sheet = "new1", startRow = a,header=FALSE)
	saveWorkbook(wb)
	svalue(txt_Token)<-""
	svalue(txt_name)<-""
	svalue(txt_email)<-""
	svalue(radio_gender)<-"Male"
	svalue(txt_city)<-""
	svalue(txt_state)<-""
	svalue(txt_country)<-""
	svalue(txt_occ)<-""
	svalue(txt_clgcomName)<-""
	svalue(event1)<-FALSE
	svalue(event2)<-FALSE
	svalue(event3)<-FALSE
	svalue(event4)<-FALSE
	svalue(event5)<-FALSE
	svalue(event6)<-FALSE
	svalue(event7)<-FALSE
	svalue(event8)<-FALSE
	svalue(event9)<-FALSE
	svalue(event10)<-FALSE
	svalue(event11)<-FALSE
	svalue(event12)<-FALSE
}

addHandlerClicked ( btn_clear, do_clear)
addHandlerClicked ( btn_check, do_check)
addHandlerClicked ( btn_Submit, do_submit)
