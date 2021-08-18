# Platanos
Análisis de la evolución del precio del plátano

* Datos de precios del plátano extraídos de [Agricultura del Gobierno de Canarias](https://www3.gobiernodecanarias.org/agricultura/statistical-visualizer/data.html?resourceType=dataset&agencyId=ISTAC&resourceId=C00014A_000011&version=1.5&multidatasetId=ISTAC:C00014A_000002#visualization/table)
* Datos de exportaciones en Toneladas, extraídos de [ISTAC](http://www.gobiernodecanarias.org/istac/jaxi-istac/tabla.do)

Se crean 3 archivos de datos procesados:
* Precios semanales para Tenerife pr_tfe (año, semana, precio E_kg)
* Precios mensuales para Tenerife pr_tfe_mes (fecha, pmp E_kg). Se toma el primer dia del mes, pero es el mes completo
* Importaciones mensuales para Tenerife imp_tfe (fecha, pen, extr). Se toma el primer dia del mes, pero es el mes completo. *pen* implica peninsular y *extr* extrapeninsular. Medidas en Toneladas.
