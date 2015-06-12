library("gWidgets2")
library("gWidgets2tcltk")
library("XLConnect")
rwindow <- gwindow(title="Register Window")
main <- gframe("Registration Form", container = rwindow)
tbl<-glayout(container=main) 
tbl[1,1]<-"Name: "
tbl[1,2]<-(txt_name <- gedit(container=tbl))
tbl[2,1]<-"Email: "
tbl[2,2]<-(txt_email <- gedit(container=tbl))
tbl[3,1]<-"Gender: "
tbl[3,2]<-(radio_gender <- gradio(c("Male","Female"),container=tbl))
tbl[4,1]<-"City: "
tbl[4,2]<-(txt_city <- gedit(container=tbl,text="Pune"))
tbl[5,1]<-"State: "
tbl[5,2]<-(txt_state <- gedit(container=tbl,text="Maharashtra"))
tbl[6,1]<-"Country: "
tbl[6,2]<-(txt_country <- gedit(container=tbl,text="India"))
tbl[7,1]<-"Occupation: "
tbl[7,2]<-(txt_occ <- gcombobox(c("Student Under Graduate","Student Post Graduate","Professional Academics","Professional IT","Professional NonIT","Other"),container=tbl))
tbl[8,1]<-"College/Company Name: "
tbl[8,2]<-(txt_clgcomName <- gedit(container=tbl))
tbl[9,1]<-"Tracks: "
tbl[10,1]<-(event1 <- gcheckbox(text="Big Data",container=tbl))
tbl[10,2]<-(event2 <- gcheckbox(text="Drupal",container=tbl))
tbl[11,1]<-(event3 <- gcheckbox(text="Embedded Technologies",container=tbl))
tbl[11,2]<-(event4 <- gcheckbox(text="FOSS General",container=tbl))
tbl[12,1]<-(event5 <- gcheckbox(text="Mobile technologies",container=tbl))
tbl[12,2]<-(event6 <- gcheckbox(text="Mozilla",container=tbl))
tbl[13,1]<-(event7 <- gcheckbox(text="OpenStack Miniconf",container=tbl))
tbl[13,2]<-(event8 <- gcheckbox(text="Python",container=tbl))
tbl[14,1]<-(event9 <- gcheckbox(text="Systemadmin & Security",container=tbl))
tbl[14,2]<-(event10 <- gcheckbox(text="Web technologies",container=tbl))
tbl[15,1]<-(event11 <- gcheckbox(text="Community Events",container=tbl))
tbl[15,2]<-(event12 <- gcheckbox(text="Other",container=tbl))
tbl[16,1]<-(btn_clear <- gbutton(container=tbl,text="Clear"))
tbl[16,2]<-(btn_Submit <- gbutton(container=tbl,text="Submit"))

do_clear <- function(obj){ 
  svalue(txt_name)<-""
  svalue(txt_email)<-""
  svalue(radio_gender)<-"Male"
  svalue(txt_city)<-"Pune"
  svalue(txt_state)<-"Maharashtra"
  svalue(txt_country)<-"India"
  svalue(txt_occ)<-"Student Under Graduate"
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

do_submit<-function(obj){
	wb <- loadWorkbook("gnunify.xlsx")
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

	udata<-data.frame(uname,uemail,ugender,ucity,ustate,ucountry,uocc,uclgname,u1event1,u1event2,u1event3,u1event4,u1event5,u1event6,u1event7,u1event8,u1event9,u1event10,u1event11,u1event12)
	writeWorksheet(wb, udata, sheet = "new1", startRow = a,header=FALSE)
	saveWorkbook(wb)
	  svalue(txt_name)<-""
  svalue(txt_email)<-""
  svalue(radio_gender)<-"Male"
  svalue(txt_city)<-"Pune"
  svalue(txt_state)<-"Maharashtra"
  svalue(txt_country)<-"India"
  svalue(txt_occ)<-"Student Under Graduate"
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
addHandlerClicked ( btn_Submit, do_submit)
