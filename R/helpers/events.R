events_magnifier <- function(item_data,template){
  if(nrow(item_data)>1){
    item_data = item_data[1,]
  }
  if(template == "chartevents"){
    chartevents_to_html(item_data)
  } else if(template == "inputevents"){
    inputevents_to_html(item_data)
  } else if(template == "procedureevents"){
    procedureevents_to_html(item_data)
  } else if(template == "datetimeevents"){
    datetimeevents_to_html(item_data)
  } else if(template == "outputevents"){
    outputevents_to_html(item_data)
  } else if(template == "ingredientevents"){
    ingredientevents_to_html(item_data)
  } else if(template == "labevents"){
    labevents_to_html(item_data)
  } else if(template == "microbiologyevents"){
    microbiologyevents_to_html(item_data)
  } else if(template == "prescriptions"){
    prescriptions_to_html(item_data)
  } else if(template == "customevents"){
    customevents_to_html(item_data)
  }else if(template == "demographics"){
    demographics_to_html(item_data)
  }else{
    tags$div("Preview is not implemented yet for this item")
  }

}

chartevents_to_html <- function(item_data){
  tags$div(tags$div(tags$b("Item ID : "),item_data[["itemid"]]),
      tags$div(tags$b("Label : "),item_data[["label"]]),
      tags$div(tags$b("Category : "),item_data[["category"]]),
      tags$div(tags$b("Parameter type : "),item_data[["param_type"]]),
      tags$div(tags$b("Numerical value : "),item_data[["valuenum"]]),
      tags$div(tags$b("Text value : "),item_data[["value"]]),
      tags$div(tags$b("Unit of measure : "),item_data[["valueuom"]]),
      tags$div(tags$b("Chart time : "),item_data[["charttime"]]),
      tags$div(tags$b("From table : "),"chartevents"),
      tags$div(class="ui divider"),
      style="display:flex;flex-direction:column;")
}

inputevents_to_html <- function(item_data){
  tags$div(tags$div(tags$b("Item ID : "),item_data[["itemid"]]),
      tags$div(tags$b("Label : "),item_data[["label"]]),
      tags$div(tags$b("Category : "),item_data[["category"]]),
      tags$div(tags$b("Parameter type : "),item_data[["param_type"]]),
      tags$div(tags$b("Amount : "),round(item_data[["amount"]],4)),
      tags$div(tags$b("Amount unit of measure : "),item_data[["amountuom"]]),
      tags$div(tags$b("Rate : "),round(item_data[["rate"]],4)),
      tags$div(tags$b("Rate unit of measure : "),item_data[["rateuom"]]),
      tags$div(tags$b("Start time : "),item_data[["starttime"]]),
      tags$div(tags$b("End time : "),item_data[["endtime"]]),
      tags$div(tags$b("Order category name : "),item_data[["ordercategoryname"]]),
      tags$div(tags$b("Secondary order category name : "),item_data[["secondaryordercategoryname"]]),
      tags$div(tags$b("Order component type description : "),item_data[["ordercomponenttypedescription"]]),
      tags$div(tags$b("Order category description : "),item_data[["ordercategorydescription"]]),
      tags$div(tags$b("Order ID : "),item_data[["orderid"]]),
      tags$div(tags$b("Link order ID : "),item_data[["linkorderid"]]),
      tags$div(tags$b("Patient weight : "),item_data[["patientweight"]]),
      tags$div(tags$b("Is open bag : "),item_data[["isopenbag"]]),
      tags$div(tags$b("Total amount : "),item_data[["totalamount"]]),
      tags$div(tags$b("Total amount unit of measure : "),item_data[["totalamountuom"]]),
      tags$div(tags$b("Continue in next dept. : "),item_data[["continueinnextdept"]]),
      tags$div(tags$b("Status description : "),item_data[["statusdescription"]]),
      tags$div(tags$b("Original amount : "),round(item_data[["originalamount"]],4)),
      tags$div(tags$b("Original rate : "),round(item_data[["originalrate"]],4)),
      tags$div(tags$b("From table : "),"inputevents"),
      tags$div(class="ui divider"),
      style="display:flex;flex-direction:column;")
}

procedureevents_to_html <- function(item_data){
  tags$div(tags$div(tags$b("Item ID : "),item_data[["itemid"]]),
      tags$div(tags$b("Label : "),item_data[["label"]]),
      tags$div(tags$b("Category : "),item_data[["category"]]),
      tags$div(tags$b("Parameter type : "),item_data[["param_type"]]),
      tags$div(tags$b("Value : "),item_data[["value"]]),
      tags$div(tags$b("Value unit of measure : "),item_data[["valueuom"]]),
      tags$div(tags$b("Location : "),item_data[["location"]]),
      tags$div(tags$b("Location category : "),item_data[["locationcategory"]]),
      tags$div(tags$b("Start time : "),item_data[["starttime"]]),
      tags$div(tags$b("End time : "),item_data[["endtime"]]),
      tags$div(tags$b("Order ID : "),item_data[["orderid"]]),
      tags$div(tags$b("Link order ID : "),item_data[["linkorderid"]]),
      tags$div(tags$b("Order category name : "),item_data[["ordercategoryname"]]),
      tags$div(tags$b("Order category description : "),item_data[["ordercategorydescription"]]),
      tags$div(tags$b("Patient weight : "),item_data[["patientweight"]]),
      tags$div(tags$b("Is open bag : "),item_data[["isopenbag"]]),
      tags$div(tags$b("Continue in next dept. : "),item_data[["continueinnextdept"]]),
      tags$div(tags$b("Status description : "),item_data[["statusdescription"]]),
      tags$div(tags$b("Original amount : "),item_data[["originalamount"]]),
      tags$div(tags$b("Original rate : "),item_data[["originalrate"]]),
      tags$div(tags$b("From table : "),"procedureevents"),
      tags$div(class="ui divider"),
      style="display:flex;flex-direction:column;")
}

datetimeevents_to_html <- function(item_data){
  tags$div(tags$div(tags$b("Item ID : "),item_data[["itemid"]]),
      tags$div(tags$b("Label : "),item_data[["label"]]),
      tags$div(tags$b("Category : "),item_data[["category"]]),
      tags$div(tags$b("Parameter type : "),item_data[["param_type"]]),
      tags$div(tags$b("Value : "),item_data[["value"]]),
      tags$div(tags$b("Unit of measure : "),item_data[["valueuom"]]),
      tags$div(tags$b("Chart time : "),item_data[["charttime"]]),
      tags$div(tags$b("From table : "),"datetimeevents"),
      tags$div(class="ui divider"),
      style="display:flex;flex-direction:column;")
}

outputevents_to_html <- function(item_data){
  tags$div(tags$div(tags$b("Item ID : "),item_data[["itemid"]]),
      tags$div(tags$b("Label : "),item_data[["label"]]),
      tags$div(tags$b("Category : "),item_data[["category"]]),
      tags$div(tags$b("Parameter type : "),item_data[["param_type"]]),
      tags$div(tags$b("Value : "),item_data[["value"]]),
      tags$div(tags$b("Unit of measure : "),item_data[["valueuom"]]),
      tags$div(tags$b("Chart time : "),item_data[["charttime"]]),
      tags$div(tags$b("From table : "),"outputevents"),
      tags$div(class="ui divider"),
      style="display:flex;flex-direction:column;")
}

ingredientevents_to_html <- function(item_data){
  tags$div(tags$div(tags$b("Item ID : "),item_data[["itemid"]]),
      tags$div(tags$b("Label : "),item_data[["label"]]),
      tags$div(tags$b("Category : "),item_data[["category"]]),
      tags$div(tags$b("Parameter type : "),item_data[["param_type"]]),
      tags$div(tags$b("Amount : "),item_data[["amount"]]),
      tags$div(tags$b("Amount unit of measure : "),item_data[["amountuom"]]),
      tags$div(tags$b("Rate : "),item_data[["rate"]]),
      tags$div(tags$b("Rate unit of measure : "),item_data[["rateuom"]]),
      tags$div(tags$b("Start time : "),item_data[["starttime"]]),
      tags$div(tags$b("End time : "),item_data[["endtime"]]),
      tags$div(tags$b("Order ID : "),item_data[["orderid"]]),
      tags$div(tags$b("Link order ID : "),item_data[["linkorderid"]]),
      tags$div(tags$b("Status description : "),item_data[["statusdescription"]]),
      tags$div(tags$b("Original amount : "),item_data[["originalamount"]]),
      tags$div(tags$b("Original rate : "),item_data[["originalrate"]]),
      tags$div(tags$b("From table : "),"ingredientevents"),
      tags$div(class="ui divider"),
      style="display:flex;flex-direction:column;")
}

labevents_to_html <- function(item_data){
  tags$div(tags$div(tags$b("Item ID : "),item_data[["itemid"]]),
      tags$div(tags$b("Label : "),item_data[["label"]]),
      tags$div(tags$b("Category : "),item_data[["category"]]),
      tags$div(tags$b("Fluid : "),item_data[["fluid"]]),
      tags$div(tags$b("Numerical value : "),item_data[["valuenum"]]),
      tags$div(tags$b("Text value : "),item_data[["value"]]),
      tags$div(tags$b("Unit of measure : "),item_data[["valueuom"]]),
      tags$div(tags$b("Chart time : "),item_data[["charttime"]]),
      tags$div(tags$b("Ref range lower : "),item_data[["ref_range_lower"]]),
      tags$div(tags$b("Ref range upper : "),item_data[["ref_range_upper"]]),
      tags$div(tags$b("Flag : "),item_data[["flag"]]),
      tags$div(tags$b("Priority : "),item_data[["priority"]]),
      tags$div(tags$b("Comments : "),item_data[["comments"]]),
      tags$div(tags$b("From table : "),"labevents"),
      tags$div(class="ui divider"),
      style="display:flex;flex-direction:column;")
}

microbiologyevents_to_html <- function(item_data){
  tags$div(tags$div(tags$b("Test ID : "),item_data[["test_itemid"]]),
           tags$div(tags$b("Test Name : "),item_data[["test_name"]]),
           tags$div(tags$b("Specimen Tested : "),item_data[["spec_type_desc"]]),
           tags$div(tags$b("Detected Organism ID : "),item_data[["org_itemid"]]),
           tags$div(tags$b("Detected Organism : "),item_data[["org_name"]]),
           tags$div(tags$b("Isolated Colony Count : "),item_data[["isolate_num"]]),
           tags$div(tags$b("Quantity : "),item_data[["quantity"]]),
           tags$div(tags$b("Tested Antibiotic ID : "),item_data[["ab_itemid"]]),
           tags$div(tags$b("Tested Antibiotic : "),item_data[["ab_name"]]),
           tags$div(tags$b("Antibiotic Dilution (text) : "),item_data[["dilution_text"]]),
           tags$div(tags$b("Antibiotic Dilution (comparison) : "),item_data[["dilution_comparison"]]),
           tags$div(tags$b("Antibiotic Dilution (value) : "),item_data[["dilution_value"]]),
           tags$div(tags$b("Antibiotic Sensitivity : "),item_data[["interpretation"]]),
           tags$div(tags$b("Comments : "),item_data[["comments"]]),
           tags$div(tags$b("From table : "),"microbiologyevents"),
           tags$div(class="ui divider"),
           style="display:flex;flex-direction:column;")
}
prescriptions_to_html <- function(item_data){
  tags$div(tags$div(tags$b("Pharmacy ID : "),item_data[["pharmacy_id"]]),
           tags$div(tags$b("POE ID : "),item_data[["poe_id "]]),
           tags$div(tags$b("Provider ID : "),item_data[["order_provider_id"]]),
           tags$div(tags$b("Start Prescription Time : "),item_data[["starttime"]]),
           tags$div(tags$b("End Prescription Time : "),item_data[["stoptime"]]),
           tags$div(tags$b("Free Text Prescription : "),item_data[["prod_strength"]]),
           tags$div(tags$b("Drug : "),item_data[["drug"]]),
           tags$div(tags$b("Drug Container : "),item_data[["form_rx"]]),
           tags$div(tags$b("Drug Type : "),item_data[["drug_type"]]),
           tags$div(tags$b("Drug Ontology  : "),item_data[["formulary_drug_cd"]]),
           tags$div(tags$b("Dose prescribed : "),item_data[["dose_val_rx"]]),
           tags$div(tags$b("Dose unit : "),item_data[["dose_unit_rx"]]),
           tags$div(tags$b("Formulary dose : "),item_data[["form_val_disp"]]),
           tags$div(tags$b("Formulary unit : "),item_data[["form_unit_disp"]]),
           tags$div(tags$b("Number of dose daily (inconsistent data) : "),item_data[["doses_per_24_hrs"]]),
           tags$div(tags$b("Route : "),item_data[["route"]]),
           tags$div(tags$b("GSN : "),item_data[["gsn"]]),
           tags$div(tags$b("NDC : "),item_data[["ndc"]]),
           tags$div(tags$b("From table : "),"prescriptions"),
           tags$div(class="ui divider"),
           style="display:flex;flex-direction:column;")
}
customevents_to_html <- function(item_data){
  tags$div(tags$div(tags$b("Item ID : "),item_data[["itemid"]]),
           tags$div(tags$b("Label : "),item_data[["label"]]),
           tags$div(tags$b("Chart time : "),item_data[["charttime"]]),
           tags$div(tags$b("Value : "),item_data[["value"]]),
           tags$div(tags$b("Unit of measure : "),item_data[["valueuom"]]),
           tags$div(tags$b("From table : "),"customevents"),
           tags$div(tags$i("This events is a custom events created by "),item_data[["author"]]),
           tags$div(class="ui divider"),
           style="display:flex;flex-direction:column;")
}

demographics_to_html <- function(item_data){
  tags$div(tags$div(tags$b("Item ID : "),item_data[["itemid"]]),
           tags$div(tags$b("Label : "),item_data[["label"]]),
           tags$div(tags$b("Chart time : "),item_data[["charttime"]]),
           tags$div(tags$b("Value : "),item_data[["value"]]),
           tags$div(tags$b("Unit of measure : "),item_data[["valueuom"]]),
           tags$div(tags$b("From table : "),"demographics"),
           tags$div(class="ui divider"),
           style="display:flex;flex-direction:column;")
}
