
WITH dep as (
	select distinct p.DepartmentKey,
		case  when DepartmentAbbreviation in ('3N-UNIV','3S-UNIV') then '3-SCCT'  
				when DepartmentAbbreviation in ('4N-CICU','4S-ICU') then '4-SCCT'  
				when DepartmentAbbreviation in ('5N-SICU','5S-SICU') then '5-SCCT'  
				when DepartmentAbbreviation in ('6N-CSICU','6S-CSICU') then '6-SCCT'  
				when DepartmentAbbreviation in ('7N-MICU','7S-RICU') then '7-SCCT'  
				when DepartmentAbbreviation in ('8N-ICU','8S-NSICU') then '8-SCCT'  
				when DepartmentAbbreviation like '%ICU' then 'ICU' 
				when DepartmentAbbreviation like '%PACU' then 'PACU' 
				when DepartmentAbbreviation in('6-ICU','6-NE','6-NW','7-NW','8-NE','8-NW','8-SW','8-SE') then 'General Surgery'     
				when DepartmentAbbreviation in ('3-N','4-NW','4-SE','4-SW','5-SE','5-SW','7-SE','7-SW') then 'Medical'  
			 	when DepartmentSpecialty like '*%' then 'Unspecified' 
				else p.DepartmentSpecialty end as DepartmentType,
			p.DepartmentAbbreviation 
	from DepartmentDim p 
)
,Pat as (  -- 47648 unique enc
	SELECT distinct de.EncounterKey, 
		de.PatientDurableKey,
		InpatientAdmissionDateKey, admitdt.DateValue as AdmitDate, 
		DischargeDateKey, dischdt.DateValue as DischargeDate, 
		dur.years as Age, 
		case when Sex like '*%' then 'Unknown' else Sex end Sex, 
		case when SmokingStatus in ( 'Heavy Tobacco Smoker', 'Current Every Day Smoker') then 'Heavy'
			when SmokingStatus in ('Former Smoker','Current Some Day Smoker','Light Tobacco Smoker','Smoker') then 'Light'
			when SmokingStatus in ('Passive Smoke Exposure - Never Smoker') then 'PassiveSmoker'
			else 'Unknown' end SmokingStatus,
		--case when p.Status like '*%' then 'Unknown' else p.Status end PatientStatus,
		case when p.MaritalStatus like '*%' then 'Unknown' else MaritalStatus end MaritalStatus, 
		case when StateOrProvinceAbbreviation = 'CA' then 1 else 0 end as InCalifornia,
		case when haf.PatientClass like '*%' then 'Unknown' else PatientClass end PatientClass,
		case
			when upper(HospitalService) like 'SUR%' then 'Surgery'
			when upper(HospitalService) like '%Transplant%' then 'Transplant'
			when upper(HospitalService) like 'REH%' then 'Rehab'
			when upper(HospitalService) like 'CAR%' then 'Cardiology'
			when upper(HospitalService) like 'OBS%' then 'Obstetrics' 
			when upper(HospitalService) like '*%'  then 'Unknwon' 
			when upper(HospitalService) like '%PSY%'  then 'Psych' 
			when upper(HospitalService) like 'MED%'  then 'Medicine' 
			when upper(HospitalService) like '%PED%'  then 'Pediatrics' 
			else HospitalService end as HospitalService,
		case when haf.AdmissionType like '*%' then 'Unknown' else AdmissionType end AdmissionType, 
		case 
			when admissionsource like '%Acute%' then 'Acute' 
			when admissionsource like '%SNF%' then 'SNF'
			when admissionsource like '%Hospice' then 'Hospice'
			when admissionsource like 'Newborn%' then 'Newborn'
			when admissionsource like 'Transfer%' then 'Transfer'
			when admissionsource like 'Home%' then 'Home'
			when admissionsource like 'MD Office%' then 'Clinic'
			else 'Other' end as  AdmissionSource, 
		case when haf.DischargeDisposition like '*%' then 'Unknown'
			when haf.DischargeDisposition like '%Readmission%' then 'Inp Readmission'
			when haf.DischargeDisposition like '%Hospice%' then 'Hospice'
			when haf.DischargeDisposition like '%Facility%' or DischargeDisposition like '%Hospital%' then 'Facility'
			when haf.DischargeDisposition like '%Rehab%' then 'Rehab'
			when haf.DischargeDisposition like '%Psych%' then 'Psych'
			when haf.DischargeDisposition like '%Home%' then 'Home'
			when haf.DischargeDisposition like '%Acute%' then 'Acute'
			when haf.DischargeDisposition like '%Deceased%' then 'Expired'
			when haf.DischargeDisposition like '%BILLING%' or DischargeDisposition like 'Left Without%' or DischargeDisposition like '%Error%' then 'Unknown'
			else haf.DischargeDisposition end as DischargeDisposition, 
		case when left(PayorFinancialClass,1) = '*' then 'OTHER'
			when PayorFinancialClass like 'PENDING%' then replace(PayorFinancialClass,'PENDING ', '') 
			when PayorFinancialClass = 'Blue Cross' then 'Commercial'
			else upper(PayorFinancialClass) end as PayorFinancialClass,
		AdmitDep.DepartmentType as AdmittingDepartmentType,
		DischDep.DepartmentType as DischargeDepartmentType
	FROM  DiagnosisTerminologyDim dt 
		inner join DiagnosisEventFact as de
		on dt.DiagnosisKey = de.DiagnosisKey
		inner join HospitalAdmissionFact as haf 
		on de.EncounterKey = haf.EncounterKey  --and haf.patientdurablekey = 4714898   ------------- commenct out patient
		inner join DurationDim dur 
		on haf.AgeKey = dur.DurationKey 
		inner join PatientDim p
		on haf.PatientDurableKey = p.DurableKey
		inner join CoverageDim cov
		on haf.PrimaryCoverageKey = cov.CoverageKey
		inner join dep AdmitDep		 
		on haf.AdmittingDepartmentKey = AdmitDep.DepartmentKey
		inner join dep DischDep
		on haf.DepartmentKey = DischDep.DepartmentKey
		inner join DateDim admitdt
		on haf.InpatientAdmissionDateKey = admitdt.DateKey
		inner join DateDim dischdt
		on haf.DischargeDateKey = dischdt.DateKey
	WHERE dt.type = 'ICD-10-CM' 
	and haf.InpatientAdmissionDateKey  between 20160101 and 20191231
	and haf.DischargeDateKey > 0 -- Discharged
	and convert(date,cast(haf.DischargeDateKey as varchar), 112) < cast( dateadd(day, -30, getdate() ) as date) --  discharge date is at least 30 days ago
	and haf.DischargeDisposition not in ('Expired','Acute','Against Medical Advice (AMA)')
	and dur.years >= 18  -- adult patients only
	and p.IsCurrent = 1
	and de.type = 'Billing Diagnosis'  and de.IdType like '%FinalDiagnosis'
	and haf.HospitalService not in ('Obstetrics Gynecology','Labor and Delivery','Newborn')  -- show accuracy incl. these
	and dt.value in (
	'I09.9','I25.5','I26.01','I26.02','I26.09','I26.90','I26.92','I26.99','I27.0','I27.1','I27.2','I27.20','I27.21','I27.22','I27.23','I27.24','I27.29','I27.81',
	'I27.82','I27.83','I27.89','I27.9','I28.0','I28.8','I28.9','I42.0','I42.5','I42.7','I42.8','I42.9','I43','I44.1','I44.2','I44.30','I44.39','I45.6','I45.9',
	'I47.0','I47.1','I47.2','I47.9','I48.0','I48.1','I48.2','I48.3','I48.4','I48.91','I48.92','I49.01','I49.02','I49.1','I49.2','I49.3','I49.40','I49.49','I49.5',
	'I49.8','I49.9','I50.1','I50.20','I50.21','I50.22','I50.23','I50.30','I50.31','I50.32','I50.33','I50.40','I50.41','I50.42','I50.43','I50.810','I50.811',
	'I50.812','I50.813','I50.814','I50.82','I50.83','I50.84','I50.89','I50.9','R00.0','R00.1','R00.8','Z45.010','Z45.018','Z45.02','Z45.09','Z95.0'
	)
, Diag_Pat_Admits as (  --29527 unique pat
	SELECT PatientDurableKey, count(distinct EncounterKey) Admits_WithSameDiagnosis
	From pat
	group by PatientDurableKey
)
, Readmit_Days as ( --61146
	SELECT	EncounterKey, --InpatientAdmissionDateKey, AdmitDate, DischargeDate,PrevDischargeDate,NextAdmitDate,
			CASE WHEN PrevDischargeDate is not null and DateDiff(Day, PrevDischargeDate, AdmitDate) <= 30 then 1 else 0 end as Readmit_Within_30Days,  -- Target Predict variable 1
			CASE WHEN PrevDischargeDate is not null then DateDiff(Day, PrevDischargeDate,  AdmitDate) else null end as ReadmitInDays ,   -- Target Predict var 2
			CASE WHEN PrevDischargeDate is NULL then 0 ELSE 1 end as PriorAdmitWithinOneYear,	 
			count(EncounterKey) OVER (Partition by PatientDurableKey) - 1 as TotalAdmits_WithinLastYear,   -- Exclude current
			round(avg(InpatientLengthOfStayInDays) OVER (Partition by PatientDurableKey) ,1) as AvgLos, 
			round(avg(InpatientLengthOfStayInDays - ExpectedInpatientLengthOfStayInDays) OVER (Partition by PatientDurableKey),1) as ExpectedMinusActualLos
	FROM (	 
		SELECT EncounterKey,
			PatientDurableKey,
			InpatientAdmissionDateKey, AdmitDate, DischargeDate,
			InpatientLengthOfStayInDays,
			ExpectedInpatientLengthOfStayInDays,
			lag(DischargeDate) over (partition by patientdurablekey order by InpatientAdmissionDateKey)  as PrevDischargeDate	
		FROM (  
			SELECT distinct haf.EncounterKey,haf.patientdurablekey,
			 haf.InpatientAdmissionDateKey, admitdt.DateValue  as AdmitDate, 
			 haf.DischargeDateKey,   dischdt.DateValue  as DischargeDate, 
			 haf.InpatientLengthOfStayInDays, haf.ExpectedInpatientLengthOfStayInDays
			FROM pat 
				inner join HospitalAdmissionFact haf
				on pat.PatientDurableKey = haf.PatientDurableKey
				and haf.InpatientAdmissionDateKey >= cast(convert(varchar,dateadd(day, -365, pat.AdmitDate),112) as bigint)  -- admits a year prior to the admission
				and haf.DischargeDateKey > 0
				and haf.InpatientAdmissionDateKey <= pat.InpatientAdmissionDateKey  -- before the current admit
				inner join DateDim admitdt		on haf.DischargeDateKey = admitdt.DateKey
				inner join DateDim dischdt		on haf.DischargeDateKey = dischdt.DateKey
			WHERE 1=1
			and haf.HospitalService not in ('Obstetrics Gynecology','Labor and Delivery','Newborn')
			--and haf.patientdurablekey = 4714898
			) s1
		)s2
)
, EDVisits as (
	SELECT ed.PatientDurableKey, count(ed.EncounterKey) EDVisits_WithinLast6Months 
	FROM pat inner join EdVisitFact ed
		on pat.PatientDurableKey = ed.PatientDurableKey
		and ed.ArrivalDateKey  >= cast(convert(varchar,dateadd(day, -183, pat.AdmitDate),112) as bigint) 
		and ed.ArrivalDateKey < pat.InpatientAdmissionDateKey
	GROUP BY ed.PatientDurableKey
)
,CanceledVisits as (-- 18514
	SELECT  pat.PatientDurableKey,
		sum(vf.Count) as OutpatientVisit_Canceled
	FROM pat   
		INNER JOIN VisitFact vf
		on pat.PatientDurableKey = vf.PatientDurableKey
	WHERE vf.AppointmentStatus in ('Canceled','No Show','Left without seen' ) 
		and vf.AppointmentDateKey >= cast(convert(varchar,dateadd(day, -365, pat.AdmitDate),112) as bigint)  -- visits a year prior to the admission 
		and vf.AppointmentDateKey  < InpatientAdmissionDateKey 
	GROUP BY pat.PatientDurableKey
)
,Transplant as (
	SELECT  pat.PatientDurableKey,
		IsNull(MAX(case when ProcedureCode = '32851' then 1 else 0 end),0)  as LungTransplant,
		IsNull(MAX(case when ProcedureCode = '33945' then 1 else 0 end),0)  as HeartTransplant
	FROM pat 
		INNER JOIN ProcedureEventFact pef 	on pat.PatientDurableKey = pef.PatientDurableKey
	WHERE pef.ProcedureCode in ('32851','33945')  
	GROUP BY pat.PatientDurableKey
)
,Vad as ( --602
	SELECT  pat.EncounterKey,
		1 as VAD  -- 608
	FROM pat 
		INNER JOIN ProcedureEventFact pef 	
			on pat.EncounterKey = pef.EncounterKey
			and pef.ProcedureStartDateKey  between pat.InpatientAdmissionDateKey and pat.DischargeDateKey 
		inner join ProcedureDim pd 
			on pef.procedurekey = pd.ProcedureKey 
			and (pd.name like '% VAD%' or pd.name like '%VENTRICULAR ASSIST DEVICE%'  ) 
	GROUP BY pat.EncounterKey
)
, Hosp_Prob as ( -- 37379
	select pat.EncounterKey, sum(def.Count) HospProblemCnt
	from pat INNER JOIN DiagnosisEventFact  def 
		on pat.EncounterKey = def.EncounterKey
	where def.type = 'Hospital Problem'
	group by pat.EncounterKey
)
, LACE_Score as ( -- 47344
	select EncounterKey,
		case when LACE_Score < 5 then 'low'
			when LACE_Score >=5 and LACE_Score < 10 then 'moderate'
			else 'high' end LACE
	from (
		select distinct pat.EncounterKey, 
			FIRST_VALUE(Value) OVER (Partition by pat.EncounterKey Order by fvf.DateKey desc) as LACE_Score
		from pat  
			inner join FlowsheetValueFact fvf
			on pat.EncounterKey = fvf.EncounterKey
			and fvf.DateKey between pat.InpatientAdmissionDateKey and pat.DischargeDateKey   -- Recorded before discharge
		where FlowsheetRowKey in (23974,24301,24103,24272,23975,26020,23976,24476,24271,24102) -- frd.DisplayName =  'LACE+ Readmission Risk'
	) sub
)
,AcuityScores as (  -- 14234 (30%)
  	SELECT distinct pat.EncounterKey, 
		first_value(wla.TotalScore) over (partition by wla.EncounterKey order by wla.ScoreInstant desc) as TotalScore,
		first_value(wla.MedicationScore) over (partition by wla.EncounterKey order by wla.ScoreInstant desc) as MedicationScore,
		first_value(wla.AssessmentScore) over (partition by wla.EncounterKey order by wla.ScoreInstant desc) as AssessmentScore, 
		first_value(wla.RiskScore) over (partition by wla.EncounterKey order by wla.ScoreInstant desc) as  RiskScore, 
		first_value(wla.OrderScore) over (partition by wla.EncounterKey order by wla.ScoreInstant desc) as OrderScore, 
		first_value(wla.LineDrainAirwayScore) over (partition by wla.EncounterKey order by wla.ScoreInstant desc) as  LineDrainAirwayScore,
		first_value(wla.ActivitiesOfDailyLivingScore) over (partition by wla.EncounterKey order by wla.ScoreInstant desc) as  ActivitiesOfDailyLivingScore
	FROM pat  
		INNER JOIN WorkloadAcuityFact wla 
		on pat.EncounterKey = wla.EncounterKey
		and wla.ScoreDateKey between pat.InpatientAdmissionDateKey and pat.DischargeDateKey  
)
--  Conversion failed when converting date and/or time from character string.
/*, LDA as (
	select   distinct  
		pat.EncounterKey,
		case when  frd.Name like '%CENTRA LINE%' then 1 else null end as CentralLine,
		case when  frd.Name like '%URINARY CATHETER%' then  1 else null end as UrinaryCatheter,
		case when  frd.Name like '%TUBE FEEDING%' or frd.Name like '%FEEDING TUBE%' then 1 else null end as FeedingTube,
		case when  frd.Name like '%NON-SURGICAL AIRWAY%' then 1 else null end as NonSurgicalAirway,
		case when  frd.Name like '% TRACH%' then 1 else null end as Trach
	from pat  
		inner join FlowsheetValueFact fvf 
			on pat.EncounterKey = fvf.EncounterKey
			and fvf.DateKey between pat.InpatientAdmissionDateKey and pat.DischargeDateKey
		inner join FlowsheetRowDim frd 
			on fvf.FlowsheetRowKey = frd.FlowsheetRowKey
			and (frd.Name like '%CENTRA LINE%' 
				or frd.Name like '%URINARY CATHETER%'
				or frd.Name like '%TUBE FEEDING%' or frd.Name like '%FEEDING TUBE%'
				or frd.Name like '%NON-SURGICAL AIRWAY%'
				or frd.Name like '% TRACH%')
		inner join FlowsheetTemplateDim ftd 
			on fvf.FlowsheetTemplateKey = ftd.FlowsheetTemplateKey
			and ftd.Name like '% LDA%'
	--where a.EncounterKey = 68202688
	
) */
, Comorbidity as (
	select * from (
		select  distinct haf.PatientDurableKey, haf.EncounterKey, 1 as DiseasePresent,
		case 
			when value in ('I09.9','I11.0','I13.0','I13.2','I25.5','I42.0','I42.5','I42.6','I42.7','I42.8','I42.9','P29.0')
 				or value like 'I43.%' or value like 'I50.%'  then 'CHF' --'CongestiveHeartFailure' 
			when value in ('I44.1','I44.2','I44.3','I45.6','I45.9','R00.0','R00.1','R00.8','T82.1','Z45.0','Z95.0')
				or value like 'I47.%' or value like 'I48.%'or value like 'I49.%' then 'CardiacArhythmias'
			when value in ('A52.0','I09.1','I09.8','Q23.0','Q23.1','Q23.2','Q23.3','Z95.2','Z95.3','Z95.4')		
				or value like 'I05.%' or value like 'I06.%' or value like 'I07.%' or value like 'I08.%' then 'ValvularDisease'
			when value in ('I28.0','I28.8','I28.9') or value like 'I26.%' or value like 'I27.%' then 'PCD'  --'PulmonaryCirculationDisorders'
			when value in ('I73.1','I73.8','I73.9','I77.1','I79.0','I79.2','K55.1','K55.8','K55.9','Z95.8','Z95.9')
			  or value like 'I70.%' or value like 'I71.%' then 'PVD' --'PeripheralVascularDisorders'
			when value like 'I10.%' then 'HyperUncomp' --'HypertensionUncomplicated'
			when value like 'I11.%' or value like 'I12.%' or value like 'I13.%' or value like 'I15.%' then 'Hypertension'
			when value in ('G04.1','G11.4','G80.1','G80.2','G83.0 - G83.4','G83.9') 
				or value like 'G81.%' or value like 'G82.%' then 'Paralysis'
			when value in ('G25.4','G25.5','G31.2','G31.8','G31.9','G93.1','G93.4','R47.0')
				or value like 'G10.%' or value like 'G13.%' or value like 'G20.%' or value like 'G22.%' or value like 'G32.%' or value like 'G35.%' or value like 'G37.%' or value like 'G40.%' or value like 'G41.%' or value like 'R56.%'
				then 'ONeuroD' --'OtherNeurologicalDisorders'
			when value in ('I27.8','I27.9','J68.4','J70.1','J70.3')
				or value like 'J40.%' or value like 'J47.%' or value like 'J60.%' or value like 'J67.%' then 'CPD' --'ChronicPulmonaryDisease'
			when value in ('E10.0','E10.1','E10.9','E11.0','E11.1','E11.9','E12.0','E12.1','E12.9','E13.0','E13.1','E13.9','E14.0','E14.1','E14.9')
			then 'DiabetesUncomp'
			when value in ('E10.2','E10.3','E10.4','E10.5','E10.6','E10.7','E10.8','E11.2','E11.3','E11.4','E11.5','E11.6','E11.7','E11.8','E12.2','E12.3','E12.4','E12.5','E12.6','E12.7','E12.8',
							'E13.2','E13.3','E13.4','E13.5','E13.6','E13.7','E13.8','E14.2','E14.3','E14.4','E14.5','E14.6','E14.7','E14.8')
			then 'DiabetesComp'		
			when value = 'E89.0' or value like 'E00.%' or value like 'E01.%' or value like 'E02.%' or value like 'E03.%' then 'Hypothyroidism'
			when value in ('I12.0','I13.1','N25.0','Z49.0','Z49.1','Z49.2','Z94.0','Z99.2') 
				or value like 'N18.%' or value like 'N19.%'  then 'RenalFailure'
			when value in ('I86.4','I98.2','K71.1','K71.3','K71.4','K71.5','K71.7','K76.0','K76.2','K76.3','K76.4','K76.5','K76.6','K76.7','K76.8','K76.9','Z94.4')
				or value like 'B18.%' or value like 'I85.%' or value like 'K70.%' or value like 'K72.%' or value like 'K73.%' or value like 'K74.%' then 'LiverDisease'
			when value in ('K25.7','K25.9','K26.7','K26.9','K27.7','K27.9','K28.7','K28.9') then 'PepticUlcerDEBleed' -- 'PepticUlcerDiseaseExcludingBleeding'
			when value like 'B20.%' or value like 'B22.%' or value like 'B24.%' then 'AIDSHIV'
			when value in ('C90.0', 'C90.2') 
				or value like 'C81.%' or value like 'C82.%' or value like 'C83.%' or value like 'C84.%' or value like 'C85.%' or value like 'C88.%' or value like 'C96.%' then 'Lymphoma'
			when value like 'C77.%' or value like 'C78.%' or value like 'C79.%' or value like 'C80.%' then 'MetastaticCancer'
			when value like 'C0%' or value like 'C1%' or value like 'C20.%' or value like 'C21.%' or value like 'C22.%' or value like 'C23.%' or value like 'C24.%' or value like 'C25.%' or value like 'C26.%' 
				or value like 'C30.%' or value like 'C31.%' or value like 'C32.%' or value like 'C33.%' or value like 'C34.%' or value like 'C37.%' or value like 'C38.%' or value like 'C39.%' or value like 'C40.%' 
				or value like 'C41.%' or value like 'C43.%' or value like 'C45.%' or value like 'C46.%' or value like 'C47.%' or value like 'C48.%' or value like 'C49.%' or value like 'C50.%'
				or value like 'C51.%' or value like 'C52.%' or value like 'C53.%' or value like 'C54.%' or value like 'C55.%' or value like 'C56.%' or value like 'C57.%' or value like 'C58.%' 
				or value like 'C6%' or value like 'C70.%' or value like 'C71.%' or value like 'C72.%'  or value like 'C73.%' or value like 'C74.%' or value like 'C75.%' or value like 'C76.%' or value like 'C97.%'
				then 'SolidTumourWM' --'SolidTumourWithoutMetastasis'
			when value in ('L94.0','L94.1','L94.3','M12.0','M12.3','M31.0','M31.1','M31.2','M31.3','M46.1','M46.8','M46.9')
				or value like 'M05.%' or value like 'M06.%' or value like 'M08.%' or value like 'M30.%' or value like 'M32.%' or value like 'M33.%' or value like 'M34.%' or value like 'M35.%' or value like 'M45.%'  
				then 'RheumatoidACD' --'RheumatoidArthritisCollagenvascularDiseases' 
			when value in ('D69.1','D69.3','D69.4','D69.5','D69.6') or value like 'D65.%' or value like 'D66.%' or value like 'D67.%' or value like 'D68.%' then 'Coagulopathy'
			when value like 'E66.%' THEN 'Obesity'
			when value in ('R63.4', 'R64') or value like 'E40.%' or value like 'E41.%' or value like 'E42.%' or value like 'E43.%' or value like 'E44.%' or value like 'E45.%' or value like 'E46.%'
				then 'WeightLoss' 
			when value = 'E22.2' or value like 'E86.%' or value like 'E87.%' then 'FluidElectD' --'FluidAndElectrolyteDisorders'
			when value = 'D50.0' then 'BloodLossAnaemia'
			when value in ('D50.8', 'D50.9') or value like 'D51.%' or value like 'D53.%' then 'DeficiencyAnaemia'
			when value in ('F10','E52','G62.1','I42.6','K29.2','K70.0','K70.3','K70.9','Z50.2','Z71.4','Z72.1') or value like 'T51.%' then 'AlcoholAbuse'
			when value in ('Z71.5', 'Z72.2') or value like 'F11.%' or value like 'F16.%' or value like 'F18.%' or value like 'F19.%'  then 'DrugAbuse'
			when value in ('F31.2', 'F31.5','F30.2') or value like 'F20.%' or value like 'F22.%' or value like 'F23%' or value like 'F24.%' or value like 'F25.%' or value like 'F28.%' or value like 'F29.%'  
				then 'Psychoses'
			when value in ('F20.4','F31.3','F31.4','F31.5','F41.2','F43.2') or value like 'F32.%' or value like 'F33.%' or value like 'F34.1' then 'Depression'  
		end ElixhauserComorbidity
	from pat  inner join DiagnosisEventFact as de 
		on pat.PatientDurableKey = de.PatientDurableKey
		inner join DiagnosisTerminologyDim dt 
 		on dt.DiagnosisKey = de.DiagnosisKey
		inner join HospitalAdmissionFact as haf
		on de.EncounterKey = haf.EncounterKey
	where dt.type = 'ICD-10-CM' 
	) sub1
	pivot (
		max(DiseasePresent)  
		For ElixhauserComorbidity in (
			[CHF],[CardiacArhythmias],[ValvularDisease],[PCD],[PVD],[HyperUncomp],[Hypertension],[Paralysis],[ONeuroD],[CPD],[DiabetesUncomp],[DiabetesComp],
			[Hypothyroidism],[RenalFailure],[LiverDisease],[PepticUlcerDEBleed],[AIDSHIV],[MetastaticCancer],[SolidTumourWM],[RheumatoidACD],[Coagulopathy],
			[Obesity],[WeightLoss],[FluidElectD],[BloodLossAnaemia],[DeficiencyAnaemia],[AlcoholAbuse],[DrugAbuse],[Psychoses],[Depression] )
	) sub2
) 
SELECT DISTINCT
	isnull(rd.Readmit_Within_30Days,0) Readmit_Within_30Days,  --Target 1
	isnull(ReadmitInDays,0) ReadmitInDays,	-- Target 2
	isnull(PriorAdmitWithinOneYear,0) PriorAdmitWithinOneYear,
	isnull(TotalAdmits_WithinLastYear,0) TotalAdmits_WithinLastYear,
	isnull(AvgLos,0) AvgLos,
	isnull(ExpectedMinusActualLos,0) ExpectedMinusActualLos,
	isnull(EDVisits_WithinLast6Months,0) EDVisits_WithinLast6Months,
	pat.* ,  -- Remove Admit and Discharge Dates and keys
	isnull(dpa.Admits_WithSameDiagnosis,0) Admits_WithSameDiagnosis,
	isnull(cv.OutpatientVisit_Canceled,0) OutpatientVisit_Canceled,
	isnull(transp.LungTransplant,0) LungTransplant,
	isnull(transp.HeartTransplant,0) HeartTransplant,
	isnull(Vad.Vad,0) Vad,
	isnull(hp.HospProblemCnt, 0) HospProblemCnt,
	isnull(lace.LACE,0) LACE,
	isnull(acuity.TotalScore,0) TotalScore,
	isnull(acuity.MedicationScore,0) MedicationScore,
	isnull(acuity.AssessmentScore,0) AssessmentScore,
	isnull(acuity.RiskScore,0) RiskScore,
	isnull(acuity.OrderScore,0) OrderScore,
	isnull(acuity.LineDrainAirwayScore,0) LineDrainAirwayScore,
	isnull(acuity.ActivitiesOfDailyLivingScore,0) ActivitiesOfDailyLivingScore, 
	isnull(com.CHF,0) CHF,
	isnull(com.CardiacArhythmias,0) CardiacArhythmias,
	isnull(com.ValvularDisease,0) ValvularDisease,
	isnull(com.PCD,0) PCD,
	isnull(com.PVD, 0) PVD,  -- ?
	isnull(com.HyperUncomp, 0) HyperUncomp,
	isnull(com.Hypertension, 0) Hypertension,
	isnull(com.Paralysis, 0) Paralysis,
	isnull(com.CPD, 0) CPD,
	isnull(com.ONeuroD, 0) ONeuroD,
	isnull(com.DiabetesUncomp,0) DiabetesUncomp,
	isnull(com.DiabetesComp, 0) DiabetesComp,
	isnull(com.Hypothyroidism,0) Hypothyroidism,
	isnull(com.RenalFailure,0) RenalFailure,
	isnull(com.LiverDisease,0) LiverDisease,
	isnull(com.PepticUlcerDEBleed, 0) PepticUlcerDEBleed,
	isnull(com.AIDSHIV, 0) AIDSHIV,
	isnull(com.MetastaticCancer, 0) MetastaticCancer,
	isnull(com.SolidTumourWM,0) SolidTumourWM,
	isnull(com.RheumatoidACD,0) RheumatoidACD,
	isnull(com.Coagulopathy, 0) Coagulopathy,
	isnull(com.Obesity, 0) Obesity,
	isnull(com.WeightLoss,0) WeightLoss,
	isnull(com.FluidElectD,0) FluidElectD,
	isnull(com.BloodLossAnaemia, 0) BloodLossAnaemia,
	isnull(com.DeficiencyAnaemia, 0) DeficiencyAnaemia,
	isnull(com.AlcoholAbuse, 0) AlcoholAbuse,
	isnull(com.DrugAbuse, 0) DrugAbuse,
	isnull(com.Psychoses, 0) Psychoses,
	isnull(com.Depression, 0) Depression	 
FROM pat
	left outer join Diag_Pat_Admits dpa on pat.PatientDurableKey = dpa.PatientDurableKey
	left outer join Readmit_Days rd on pat.EncounterKey = rd.EncounterKey
	left outer join EDVisits ed on pat.PatientDurableKey = ed.PatientDurableKey
	left outer join CanceledVisits cv on pat.PatientDurableKey = cv.PatientDurableKey
	left outer join Transplant transp on pat.PatientDurableKey = transp.PatientDurableKey
	left outer join Vad  on pat.EncounterKey = Vad.EncounterKey
	left outer join Hosp_Prob hp on pat.EncounterKey = hp.EncounterKey
	left outer join LACE_Score lace on pat.EncounterKey = lace.EncounterKey
	left outer join AcuityScores acuity on pat.EncounterKey = acuity.EncounterKey 
--	left outer join LDA  on pat.EncounterKey = LDA.EncounterKey 
	left outer join Comorbidity com on pat.EncounterKey = com.EncounterKey  

	