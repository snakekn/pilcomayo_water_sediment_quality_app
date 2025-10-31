param_mapping <- list(
  # Basic Information
  "Estación" = "Station",
  "Código" = "Code",
  "Fecha" = "Date",
  "Hora" = "Time",
  "Campaña" = "Campaign",
  "Responsable" = "Data Controller",
  "Institución" = "Institution",
  "Río" = "River",
  "Cuenca" = "Basin",
  "Latitud" = "Latitude",
  "Longitud" = "Longitude",
  "Latitud Decimal" = "Decimal latitude",
  "Longitud Decimal" = "Decimal longitude",
  
  # Physical Parameters
  "Velocidad Media (m/s)" = "Average Velocity (m/s)",
  "Caudal (m3/s)" = "Flow (m3/s)",
  "Resistividad (Ohm.cm)" = "Resistivity (Ohm.cm)",
  "Presión Parcial (mm Hg)" = "Partial Pressure (mm Hg)",
  "pH (u pH)" = "pH (u pH)",
  "pH (mV) (mV)" = "pH (mV) (mV)",
  "Conductividad específica (T ref. 25°C) (uS/cm)" = "Specific conductivity (T ref. 25°C) (uS/cm)",
  "Salinidad (Sal)" = "Salinity (Sal)",
  "Temperatura (C)" = "Temperature (C)",
  
  # Oxygen Parameters
  "Oxígeno disuelto (mg/l O2)" = "Oxygen dissolved (mg/l O2)",
  "Saturación de oxígeno (%)" = "Oxygen Saturation (%)",
  
  # Solids and Color
  "Turbiedad (UNT)" = "Turbidity (UNT)",
  "Sólidos totales (mg/l)" = "Solids total (mg/l)",
  "Sólidos disueltos totales (mg/l)" = "Dissolved Solids total (mg/l)",
  "Sólidos suspendidos totales (mg/l)" = "Suspended solids total (mg/l)",
  "Color (u PtCo)" = "Color (u PtCo)",
  
  # Hardness and Alkalinity
  "Dureza total (mg/l CaCO3)" = "Total hardness (mg/l CaCO3)",
  "Alcalinidad Fenolftaleina (mg/l CaCO3)" = "Phenolphthalein alkalinity (mg/l CaCO3)",
  "Alcalinidad total (mg/l CaCO3)" = "Total alkalinity (mg/l CaCO3)",
  
  # Anions
  "Cloruros (mg/l Cl -)" = "Chlorides (mg/l Cl -)",
  "Sulfatos (mg/l SO4=)" = "Sulphates (mg/l SO4=)",
  
  # Major Cations - Dissolved
  "Calcio disuelto (mg/l Ca)" = "Calcium dissolved (mg/l Ca)",
  "Magnesio disuelto (mg/l Mg)" = "Magnesium dissolved (mg/l Mg)",
  "Sodio disuelto (mg/l Na)" = "Sodium dissolved (mg/l Na)",
  "Potasio disuelto (mg/l K)" = "Potassium dissolved (mg/l K)",
  
  # Major Cations - Total
  "Calcio total (mg/l Ca)" = "Calcium total (mg/l Ca)",
  "Magnesio total (mg/l Mg)" = "Magnesium total (mg/l Mg)",
  "Sodio total (mg/l Na)" = "Sodium total (mg/l Na)",
  "Potasio total (mg/l K)" = "Potassium total (mg/l K)",
  
  # Nutrients
  "Fósforo total (mg/l PO4)" = "Phosphorus total (mg/l PO4)",
  "Fósforo disuelto (mg/l PO4)" = "Phosphorus dissolved (mg/l PO4)",
  "Nitrógeno amoniacal (mg/l NH4)" = "Ammonia nitrogen (mg/l NH4)",
  "Nitrito (mg/l NO2)" = "Nitrite (mg/l NO2)",
  "Nitrato (mg/l NO3)" = "Nitrates (mg/l NO3)",
  "Nitrógeno total Kjeldahl (mg/l N)" = "Total Kjeldahl nitrogen (mg/l N)",
  
  # Oxygen Demand and Organic Carbon
  "Demanda bioquímica de oxígeno (mg/l O2)" = "Biochemical oxygen demand (mg/l O2)",
  "Demanda química de oxígeno (mg/l O2)" = "Chemical oxygen demand (mg/l O2)",
  "Carbono organico disuelto (mg/l)" = "Organic carbon dissolved (mg/l)",
  
  # Microbiological Parameters
  "Coliformes totales (en UFC) (UFC/100 ml)" = "Total coliforms (CFU) (CFU/100 ml)",
  "Coliformes fecales (en UFC) (UFC/100 ml)" = "Fecal coliforms (CFU) (CFU/100 ml)",
  "Coliformes totales (en NMP) (NMP/100 ml)" = "Total coliforms (MPN) (MPN/100 ml)",
  "Coliformes fecales (en NMP) (NMP/100 ml)" = "Fecal coliforms (MPN) (MPN/100 ml)",
  "Aerobios mesófilos totales (en UFC) (UFC/100 ml)" = "Mesophilic aerobes total (CFU) (CFU/100 ml)",
  
  # Trace Metals - Dissolved
  "Bismuto disuelto (ug/l Bi)" = "Bismuth dissolved (ug/l Bi)",
  "Talio disuelto (ug/l Tl)" = "Thallium dissolved (ug/l Tl)",
  "Cromo disuelto (ug/l Cr)" = "Chromium dissolved (ug/l Cr)",
  "Manganeso disuelto (ug/l Mn)" = "Manganese dissolved (ug/l Mn)",
  "Niquel disuelto (ug/l Ni)" = "Nickel dissolved (ug/l Ni)",
  "Cobre disuelto (ug/l Cu)" = "Copper dissolved (ug/l Cu)",
  "Zinc disuelto (ug/l Zn)" = "Zinc dissolved (ug/l Zn)",
  "Arsenico disuelto (ug/l As)" = "Arsenic dissolved (ug/l As)",
  "Cadmio disuelto (ug/l Cd)" = "Cadmium dissolved (ug/l Cd)",
  "Plomo disuelto (ug/l Pb)" = "Lead dissolved (ug/l Pb)",
  "Plata disuelto (ug/l Ag)" = "Silver dissolved (ug/l Ag)",
  "Mercurio disuelto (ug/l Hg)" = "Mercury dissolved (ug/l Hg)",
  "Hierro disuelto (ug/l Fe)" = "Iron dissolved (ug/l Fe)",
  "Boro disuelto (ug/l B)" = "Boron dissolved (ug/l B)",
  "Selenio disuelto (ug/l Se)" = "Selenium dissolved (ug/l Se)",
  
  # Trace Metals - Total
  "Cromo total (ug/l Cr)" = "Chromium total (ug/l Cr)",
  "Manganeso total (ug/l Mn)" = "Manganese total (ug/l Mn)",
  "Niquel total (ug/l Ni)" = "Nickel total (ug/l Ni)",
  "Cobre total (ug/l Cu)" = "Copper total (ug/l Cu)",
  "Zinc total (ug/l Zn)" = "Zinc total (ug/l Zn)",
  "Arsenico total (ug/l As)" = "Arsenic total (ug/l As)",
  "Cadmio total (ug/l Cd)" = "Cadmium total (ug/l Cd)",
  "Plomo total (ug/l Pb)" = "Lead total (ug/l Pb)",
  "Plata total (ug/l Ag)" = "Silver total (ug/l Ag)",
  "Mercurio total (ug/l Hg)" = "Mercury total (ug/l Hg)",
  "Hierro total (ug/l Fe)" = "Iron total (ug/l Fe)",
  "Boro total (ug/l B)" = "Boron total (ug/l B)",
  "Selenio total (ug/l Se)" = "Selenium total (ug/l Se)",
  "Bismuto total (ug/l Bi)" = "Bismuth total (ug/l Bi)",
  "Talio total (ug/l Tl)" = "Thallium total (ug/l Tl)",
  
  # Other Chemical Parameters
  "Cianuros (mg/l CN-)" = "Cyanides (mg/l CN-)",
  "Cianuros libre (mg/l CN-)" = "Free cyanides (mg/l CN-)",
  "Sulfuros (mg/l S=)" = "Sulphides (mg/l S=)",
  "Fenoles (mg/l C6H6O)" = "Phenols (mg/l C6H6O)",
  "Hidrocarburos totales (mg/l)" = "Hydrocarbons total (mg/l)",
  
  # Suspended Sediment Metals
  "Talio en suspensión (mg/kg Tl)" = "Thallium suspended (mg/kg Tl)",
  "Selenio suspensión (mg/kg Se)" = "Selenium suspended (mg/kg Se)",
  "Zinc en suspensión (mg/kg Zn)" = "Zinc suspended (mg/kg Zn)",
  "Plomo suspensión (mg/kg Pb)" = "Lead suspended (mg/kg Pb)",
  "Arsenico suspensión (mg/kg As)" = "Arsenic suspended (mg/kg As)",
  "Bismuto suspensión (mg/kg Bi)" = "Bismuth suspended (mg/kg Bi)",
  "Boro suspensión (mg/kg B)" = "Boron suspended (mg/kg B)",
  "Cadmio suspensión (mg/kg Cd)" = "Cadmium suspended (mg/kg Cd)",
  "Cobre suspensión (mg/kg Cu)" = "Copper suspended (mg/kg Cu)",
  "Cromo suspensión (mg/kg Cr)" = "Chromium suspended (mg/kg Cr)",
  "Hierro suspensión (mg/kg Fe)" = "Iron suspended (mg/kg Fe)",
  "Manganeso suspensión (mg/kg Mn)" = "Manganese suspended (mg/kg Mn)",
  "Niquel suspensión (mg/kg Ni)" = "Nickel suspended (mg/kg Ni)",
  "Plata suspensión (mg/kg Ag)" = "Silver suspended (mg/kg Ag)"
)

param_mapping_sed <- list(
  
  # Basic Information
  "Estación" = "Station",
  "Fecha" = "Date",
  "Hora" = "Time",
  "Campaña" = "Campaign",
  "Institución" = "Institution",
  "Río" = "River",
  "Latitud" = "Latitude",
  "Longitud" = "Longitude",
  "Distancia al margen" = "Distance from Bank",
  
  # Physical/Chemical Characteristics
  "Conductividad en pasta" = "Conductivity (uS/cm)",
  "Densidad Aparente" = "Bulk Density (g/cm3)",
  "Densidad Real" = "Real Density (g/cm3)",
  "Humedad" = "Moisture (%)",
  "Materia Orgánica" = "Organic Matter (%)",
  "pH en pasta" = "pH",
  "Velocidad Media" = "Average Velocity (m/s)",
  "Caudal" = "Flow (m3/s)",
  
  # Textural Analysis
  "Arena" = "Sand (%)",
  "Limo" = "Silt (%)",
  "Arcilla" = "Clay (%)",
  "Clasificación textural" = "Textural Classification",
  
  # Grain Size Distribution
  "0.016 mm" = "0.016 mm (%)",
  "0.032 mm - N° 450 (ASTM)" = "0.032 mm - No. 450 (ASTM) (%)",
  "0.063 mm - N° 230 (ASTM)" = "0.063 mm - No. 230 (ASTM) (%)",
  "0.125 mm - N° 120 (ASTM)" = "0.125 mm - No. 120 (ASTM) (%)",
  "0.250 mm - N° 060 (ASTM)" = "0.250 mm - No. 060 (ASTM) (%)",
  "0.500 mm - N° 035 (ASTM)" = "0.500 mm - No. 035 (ASTM) (%)",
  "1.00 mm - N° 018 (ASTM)" = "1.00 mm - No. 018 (ASTM) (%)",
  "2.00 mm - N° 010 (ASTM)" = "2.00 mm - No. 010 (ASTM) (%)",
  "4.75 mm - N° 004 (ASTM)" = "4.75 mm - No. 004 (ASTM) (%)",
  "Residuo" = "Residue (%)",
  
  # Metals - Major Elements (g/100g)
  "Aluminio (g/100 g fraccion)" = "Aluminum (g/100 g fraction) (% Al)",
  "Azufre (g/100 g fraccion)" = "Sulfur (g/100 g fraction) (% S)",
  "Calcio (g/100 g fraccion)" = "Calcium (g/100 g fraction) (% Ca)",
  "Fósforo (g/100 g fraccion)" = "Phosphorus (g/100 g fraction) (% P)",
  "Hierro (g/100 g fraccion)" = "Iron (g/100 g fraction) (% Fe)",
  "Magnesio (g/100 g fraccion)" = "Magnesium (g/100 g fraction) (% Mg)",
  "Potasio (g/100 g fraccion)" = "Potassium (g/100 g fraction) (% K)",
  "Sodio (g/100 g fraccion)" = "Sodium (g/100 g fraction) (% Na)",
  "Titanio (g/100 g fraccion)" = "Titanium (g/100 g fraction) (% Ti)",
  
  # Metals - Trace Elements (mg/kg)
  "Antimonio (mg/kg fraccion)" = "Antimony (mg/kg Sb)",
  "Arsénico (mg/kg fraccion)" = "Arsenic (mg/kg As)",
  "Bario (mg/kg fraccion)" = "Barium (mg/kg Ba)",
  "Berilio (mg/kg fraccion)" = "Beryllium (mg/kg Be)",
  "Bismuto (mg/kg fraccion)" = "Bismuth (mg/kg Bi)",
  "Boro (mg/kg fraccion)" = "Boron (mg/kg B)",
  "Cadmio (mg/kg fraccion)" = "Cadmium (mg/kg Cd)",
  "Cerio (mg/kg fraccion)" = "Cerium (mg/kg Ce)",
  "Cesio (mg/kg fraccion)" = "Cesium (mg/kg Cs)",
  "Cobalto (mg/kg fraccion)" = "Cobalt (mg/kg Co)",
  "Cobre (mg/kg fraccion)" = "Copper (mg/kg Cu)",
  "Cromo (mg/kg fraccion)" = "Chromium (mg/kg Cr)",
  "Disprosio (mg/kg fraccion)" = "Dysprosium (mg/kg Dy)",
  "Erbio (mg/kg fraccion)" = "Erbium (mg/kg Er)",
  "Escandio (mg/kg fraccion)" = "Scandium (mg/kg Sc)",
  "Estaño (mg/kg fraccion)" = "Tin (mg/kg Sn)",
  "Estroncio (mg/kg fraccion)" = "Strontium (mg/kg Sr)",
  "Europio (mg/kg fraccion)" = "Europium (mg/kg Eu)",
  "Gadolinio (mg/kg fraccion)" = "Gadolinium (mg/kg Gd)",
  "Galio (mg/kg fraccion)" = "Gallium (mg/kg Ga)",
  "Germanio (mg/kg fraccion)" = "Germanium (mg/kg Ge)",
  "Hafnio (mg/kg fraccion)" = "Hafnium (mg/kg Hf)",
  "Holmio (mg/kg fraccion)" = "Holmium (mg/kg Ho)",
  "Indio (mg/kg fraccion)" = "Indium (mg/kg In)",
  "Iterbio (mg/kg fraccion)" = "Ytterbium (mg/kg Yb)",
  "Itrio (mg/kg fraccion)" = "Yttrium (mg/kg Y)",
  "Lantano (mg/kg fraccion)" = "Lanthanum (mg/kg La)",
  "Litio (mg/kg fraccion)" = "Lithium (mg/kg Li)",
  "Lutecio (mg/kg fraccion)" = "Lutetium (mg/kg Lu)",
  "Manganeso (mg/kg fraccion)" = "Manganese (mg/kg Mn)",
  "Molibdeno (mg/kg fraccion)" = "Molybdenum (mg/kg Mo)",
  "Neodimio (mg/kg fraccion)" = "Neodymium (mg/kg Nd)",
  "Niobio (mg/kg fraccion)" = "Niobium (mg/kg Nb)",
  "Níquel (mg/kg fraccion)" = "Nickel (mg/kg Ni)",
  "Plata (mg/kg fraccion)" = "Silver (mg/kg Ag)",
  "Plomo (mg/kg fraccion)" = "Lead (mg/kg Pb)",
  "Praseodimio (mg/kg fraccion)" = "Praseodymium (mg/kg Pr)",
  "Renio (mg/kg fraccion)" = "Rhenium (mg/kg Re)",
  "Rubidio (mg/kg fraccion)" = "Rubidium (mg/kg Rb)",
  "Samario (mg/kg fraccion)" = "Samarium (mg/kg Sm)",
  "Selenio (mg/kg fraccion)" = "Selenium (mg/kg Se)",
  "Talio (mg/kg fraccion)" = "Thallium (mg/kg Tl)",
  "Tantalio (mg/kg fraccion)" = "Tantalum (mg/kg Ta)",
  "Telurio (mg/kg fraccion)" = "Tellurium (mg/kg Te)",
  "Terbio (mg/kg fraccion)" = "Terbium (mg/kg Tb)",
  "Torio (mg/kg fraccion)" = "Thorium (mg/kg Th)",
  "Tulio (mg/kg fraccion)" = "Thulium (mg/kg Tm)",
  "Uranio (mg/kg fraccion)" = "Uranium (mg/kg U)",
  "Vanadio (mg/kg fraccion)" = "Vanadium (mg/kg V)",
  "Wolframio (mg/kg fraccion)" = "Tungsten (mg/kg W)",
  "Zinc (mg/kg fraccion)" = "Zinc (mg/kg Zn)",
  "Zirconio (mg/kg fraccion)" = "Zirconium (mg/kg Zr)",
  
  # Metals - Ultra-trace Elements (ug/kg)
  "Mercurio (ug/kg fraccion)" = "Mercury (ug/kg fraction) (ug/kg Hg)",
  "Oro (ug/kg fraccion)" = "Gold (ug/kg fraction) (ug/kg Au)"
  
)
