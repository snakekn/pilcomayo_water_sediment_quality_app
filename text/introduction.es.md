### Introducción

La cuenca del río Pilcomayo, en el sur de Bolivia, ha enfrentado siglos de contaminación minera, cuyo origen se remonta al descubrimiento del yacimiento de plata más grande del mundo, cerca de Potosí, en 1545. La contaminación por metales pesados en agua y sedimentos, junto con el drenaje ácido de minas, representa riesgos significativos para el medio ambiente y para las aproximadamente 1,5 millones de personas que habitan la cuenca del Pilcomayo.

Esta herramienta permite a los usuarios explorar datos de calidad de agua y sedimentos recopilados en la cuenca entre 2016 y 2024. Los parámetros clave de calidad de agua y sedimentos se comparan con las normas de la Ley N.º 1333 de Bolivia y las Directrices de Calidad de Sedimentos del USGS cuando corresponde.

Utilice las pestañas anteriores para:

- *Filtrar* datos por tiempo, ubicación, parámetros de agua/sedimento y más.
- *Visualizar* resultados en mapas interactivos con mediciones brutas y comparaciones con estándares.
- *Explorar* tendencias de series de tiempo para estaciones de muestreo individuales y parámetros a lo largo de múltiples años.
- *Clasificar* observaciones, estaciones y parámetros por mediciones brutas y comparaciones con estándares.
- *Realizar* análisis de componentes principales (ACP) para encontrar correlaciones entre parámetros.
- *Revisar* los estándares ambientales aplicables.

------

#### Estándares Utilizados:
Calidad de Sedimentos (Directrices de Calidad de Sedimentos del USGS para vida acuática):

- ***Por debajo del TEL*** - Efectos adversos improbables/poco frecuentes
- ***Por encima del TEL*** - Efectos adversos posibles
- ***Por encima del PEL*** - Efectos adversos probables/frecuentes

Calidad del Agua (Ley General de Medio Ambiente de Bolivia, Ley N.º 1333):

- ***Clase A*** - Aguas naturales de mayor calidad, que califican como agua potable para consumo humano sin tratamiento previo, o con simple desinfección bacteriológica en casos necesarios verificados por laboratorio.
- ***Clase B*** - Aguas de uso general, que para consumo humano requieren tratamiento físico y desinfección bacteriológica.
- ***Clase C*** - Aguas de uso general, que para ser aptas para consumo humano requieren tratamiento físico-químico completo y desinfección bacteriológica.
- ***Clase D*** - Aguas de calidad mínima, que para consumo humano, en casos extremos de necesidad pública, requieren un proceso inicial de pre-sedimentación, ya que pueden tener alta turbidez por alto contenido de sólidos en suspensión, seguido de tratamiento físico-químico completo y desinfección bacteriológica especial contra huevos y parásitos intestinales.
- ***Sin clasificar*** - Supera todos los demás límites de estándares.

---

#### Notas y Advertencias:
- Los valores por encima o por debajo de los umbrales de detección se convirtieron a la mitad del umbral de detección si estaban por debajo (p. ej. '<0.5' --> '0.25'), o 1.5 veces el umbral si estaban por encima (p. ej. '>0.5' --> '0.75'). Por lo tanto, no todos los valores representan mediciones exactas.
- Algunos parámetros en los conjuntos de datos de agua y sedimentos no tienen estándares correspondientes en las Directrices USGS ni en la legislación boliviana. Por ello, algunas funciones de esta aplicación pueden no incluir el rango completo de parámetros de los datos originales, particularmente al comparar con estándares.
- Las Directrices USGS se basan en efectos sobre organismos acuáticos que viven en sedimentos, mientras que los estándares bolivianos de la Ley N.º 1333 se basan en niveles seguros para consumo/uso humano.
- El método ACP utilizado en esta aplicación rellena datos faltantes estimando valores a partir de patrones en los datos existentes. Funciona mejor cuando la mayoría de los datos están presentes y siguen tendencias claras, pero los valores completados son solo estimaciones y podrían afectar los resultados.
