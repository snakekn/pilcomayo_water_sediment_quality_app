<!DOCTYPE html>
  <html lang="en">
    <head>
    <meta charset="UTF-8">
      <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>River Remedy - Pilcomayo Basin Contamination Analysis</title>
        <script src="https://cdnjs.cloudflare.com/ajax/libs/Chart.js/3.9.1/chart.min.js"></script>
          <script src="https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.9.4/leaflet.min.js"></script>
            <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.9.4/leaflet.min.css" />
              <style>
              * {
                margin: 0;
                padding: 0;
                box-sizing: border-box;
              }
            
            body {
              font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
              background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
                                          color: #333;
                                            line-height: 1.6;
            }
            
            .header {
              background: rgba(255, 255, 255, 0.95);
              backdrop-filter: blur(10px);
              padding: 20px 0;
              box-shadow: 0 2px 20px rgba(0,0,0,0.1);
              position: sticky;
              top: 0;
              z-index: 1000;
            }
            
            .header-content {
              max-width: 1400px;
              margin: 0 auto;
              padding: 0 20px;
              display: flex;
              justify-content: space-between;
              align-items: center;
            }
            
            .logo {
              font-size: 2rem;
              font-weight: 800;
              background: linear-gradient(45deg, #667eea, #764ba2);
                                          -webkit-background-clip: text;
                                          -webkit-text-fill-color: transparent;
                                          background-clip: text;
            }
            
            .timeline-nav {
              display: flex;
              gap: 15px;
            }
            
            .timeline-btn {
              padding: 10px 20px;
              border: none;
              border-radius: 25px;
              background: rgba(102, 126, 234, 0.1);
              color: #667eea;
                font-weight: 600;
              cursor: pointer;
              transition: all 0.3s ease;
              border: 2px solid transparent;
            }
            
            .timeline-btn:hover, .timeline-btn.active {
              background: #667eea;
                color: white;
              transform: translateY(-2px);
              box-shadow: 0 5px 15px rgba(102, 126, 234, 0.3);
            }
            
            .dashboard {
              max-width: 1400px;
              margin: 30px auto;
              padding: 0 20px;
              display: grid;
              grid-template-columns: 1fr 1fr;
              grid-template-rows: auto auto auto;
              gap: 25px;
            }
            
            .panel {
              background: rgba(255, 255, 255, 0.95);
              backdrop-filter: blur(10px);
              border-radius: 20px;
              padding: 25px;
              box-shadow: 0 10px 30px rgba(0,0,0,0.1);
              transition: transform 0.3s ease, box-shadow 0.3s ease;
            }
            
            .panel:hover {
              transform: translateY(-5px);
              box-shadow: 0 20px 40px rgba(0,0,0,0.15);
            }
            
            .panel-header {
              display: flex;
              justify-content: space-between;
              align-items: center;
              margin-bottom: 20px;
              padding-bottom: 15px;
              border-bottom: 2px solid #f0f0f0;
            }
            
            .panel-title {
              font-size: 1.3rem;
              font-weight: 700;
              color: #333;
            }
            
            .risk-indicator {
              padding: 5px 12px;
              border-radius: 15px;
              font-size: 0.8rem;
              font-weight: 600;
              text-transform: uppercase;
            }
            
            .risk-safe { background: #d4edda; color: #155724; }
                .risk-moderate { background: #fff3cd; color: #856404; }
                    .risk-high { background: #f8d7da; color: #721c24; }
                        .risk-critical { background: #721c24; color: white; }
                            
                            .map-panel {
                              grid-column: 1 / -1;
                              height: 500px;
                            }
                          
                          #map {
                          height: 450px;
                          border-radius: 15px;
                        }
                      
                      .chart-container {
                        position: relative;
                        height: 350px;
                      }
                      
                      .standards-panel {
                        grid-column: 1 / -1;
                      }
                      
                      .standards-grid {
                        display: grid;
                        grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
                        gap: 15px;
                        margin-top: 20px;
                      }
                      
                      .standard-card {
                        background: linear-gradient(135deg, #f8f9fa, #e9ecef);
                                                    border-radius: 12px;
                                                    padding: 20px;
                                                    border-left: 4px solid #667eea;
                      }
                      
                      .standard-title {
                        font-weight: 600;
                        color: #495057;
                          margin-bottom: 10px;
                      }
                      
                      .standard-value {
                        font-size: 1.5rem;
                        font-weight: 700;
                        color: #667eea;
                      }
                      
                      .standard-unit {
                        font-size: 0.9rem;
                        color: #6c757d;
                      }
                      
                      .timeline-info {
                        background: rgba(102, 126, 234, 0.1);
                        border-radius: 15px;
                        padding: 20px;
                        margin-bottom: 20px;
                        border-left: 5px solid #667eea;
                      }
                      
                      .timeline-period {
                        font-size: 1.2rem;
                        font-weight: 700;
                        color: #667eea;
                          margin-bottom: 5px;
                      }
                      
                      .timeline-description {
                        color: #666;
                          font-size: 0.95rem;
                      }
                      
                      .data-summary {
                        display: grid;
                        grid-template-columns: repeat(auto-fit, minmax(120px, 1fr));
                        gap: 15px;
                        margin-top: 15px;
                      }
                      
                      .summary-item {
                        text-align: center;
                        padding: 15px;
                        background: rgba(102, 126, 234, 0.05);
                        border-radius: 10px;
                      }
                      
                      .summary-number {
                        font-size: 1.8rem;
                        font-weight: 700;
                        color: #667eea;
                      }
                      
                      .summary-label {
                        font-size: 0.8rem;
                        color: #666;
                          text-transform: uppercase;
                        font-weight: 600;
                        margin-top: 5px;
                      }
                      
                      .exceedance-bar {
                        height: 20px;
                        background: #e9ecef;
                          border-radius: 10px;
                        margin: 10px 0;
                        overflow: hidden;
                      }
                      
                      .exceedance-fill {
                        height: 100%;
                        background: linear-gradient(90deg, #28a745, #ffc107, #dc3545);
                                                    border-radius: 10px;
                                                    transition: width 0.8s ease;
                      }
                      
                      @media (max-width: 768px) {
                        .dashboard {
                          grid-template-columns: 1fr;
                          padding: 0 15px;
                        }
                        
                        .map-panel {
                          grid-column: 1;
                        }
                        
                        .standards-panel {
                          grid-column: 1;
                        }
                        
                        .timeline-nav {
                          flex-direction: column;
                          gap: 10px;
                        }
                        
                        .header-content {
                          flex-direction: column;
                          gap: 15px;
                        }
                      }
                      </style>
                        </head>
                        <body>
                        <header class="header">
                        <div class="header-content">
                        <div class="logo">River Remedy</div>
                        <nav class="timeline-nav">
                        <button class="timeline-btn active" data-period="2024">2024 Current</button>
                        <button class="timeline-btn" data-period="2011">2011 Cerro Rico</button>
                        <button class="timeline-btn" data-period="2006">2006 Baseline</button>
                        <button class="timeline-btn" data-period="all">All Periods</button>
                        </nav>
                        </div>
                        </header>
                        
                        <main class="dashboard">
                        <!-- Timeline Information Panel -->
                        <div class="panel timeline-info" id="timeline-info">
                        <div class="timeline-period">2024 Current Analysis</div>
                        <div class="timeline-description">TNC Pilcomayo.net monitoring focused on lithium mining impacts with spatial contamination mapping</div>
                        <div class="data-summary">
                        <div class="summary-item">
                        <div class="summary-number">15</div>
                        <div class="summary-label">Stations</div>
                        </div>
                        <div class="summary-item">
                        <div class="summary-number">25</div>
                        <div class="summary-label">Heavy Metals</div>
                        </div>
                        <div class="summary-item">
                        <div class="summary-number">3</div>
                        <div class="summary-label">Critical Sites</div>
                        </div>
                        <div class="summary-item">
                        <div class="summary-number">67%</div>
                        <div class="summary-label">WHO Exceeds</div>
                        </div>
                        </div>
                        </div>
                        
                        <!-- Spatial Contamination Map -->
                        <div class="panel map-panel">
                        <div class="panel-header">
                        <h3 class="panel-title">Spatial Contamination Distribution</h3>
                        <span class="risk-indicator risk-high">High Risk Area</span>
                        </div>
                        <div id="map"></div>
                        </div>
                        
                        <!-- Metal Concentrations Chart -->
                        <div class="panel">
                        <div class="panel-header">
                        <h3 class="panel-title">Heavy Metal Concentrations</h3>
                        <span class="risk-indicator risk-critical">Multiple Exceedances</span>
                        </div>
                        <div class="chart-container">
                        <canvas id="metalChart"></canvas>
                        </div>
                        </div>
                        
                        <!-- Standards Comparison Chart -->
                        <div class="panel">
                        <div class="panel-header">
                        <h3 class="panel-title">Standards Exceedance Analysis</h3>
                        <span class="risk-indicator risk-moderate">Monitoring Required</span>
                        </div>
                        <div class="chart-container">
                        <canvas id="standardsChart"></canvas>
                        </div>
                        </div>
                        
                        <!-- Regulatory Standards Reference -->
                        <div class="panel standards-panel">
                        <div class="panel-header">
                        <h3 class="panel-title">Regulatory Standards Reference</h3>
                        <span class="risk-indicator risk-safe">Reference Data</span>
                        </div>
                        <div class="standards-grid">
                        <div class="standard-card">
                        <div class="standard-title">WHO Drinking Water - Lead</div>
                        <div class="standard-value">0.01 <span class="standard-unit">mg/L</span></div>
                        </div>
                        <div class="standard-card">
                        <div class="standard-title">WHO Drinking Water - Arsenic</div>
                        <div class="standard-value">0.01 <span class="standard-unit">mg/L</span></div>
                        </div>
                        <div class="standard-card">
                        <div class="standard-title">Codex Alimentarius - Lead in Food</div>
                        <div class="standard-value">0.3 <span class="standard-unit">mg/kg</span></div>
                        </div>
                        <div class="standard-card">
                        <div class="standard-title">CDC Blood Lead Reference</div>
                        <div class="standard-value">5 <span class="standard-unit">µg/dL</span></div>
                        </div>
                        <div class="standard-card">
                        <div class="standard-title">Bolivia Law 1333 - pH</div>
                        <div class="standard-value">6-9 <span class="standard-unit">pH units</span></div>
                        </div>
                        <div class="standard-card">
                        <div class="standard-title">US EPA Soil Lead</div>
                        <div class="standard-value">70 <span class="standard-unit">mg/kg</span></div>
                        </div>
                        </div>
                        </div>
                        </main>
                        
                        <script>
                        // Sample data representing your studies
                      const studyData = {
                        '2024': {
                          title: '2024 Current Analysis',
                          description: 'TNC Pilcomayo.net monitoring focused on lithium mining impacts with spatial contamination mapping',
                          stations: 15,
                          metals: 25,
                          critical: 3,
                          exceeds: '67%',
                          riskLevel: 'high',
                          coordinates: [
                            {lat: -19.5723, lng: -65.7550, name: 'Potosí Station 1', risk: 'critical'},
                            {lat: -19.5823, lng: -65.7450, name: 'Tarapaya Station', risk: 'high'},
                            {lat: -19.5923, lng: -65.7650, name: 'Pilcomayo Station 3', risk: 'moderate'}
                          ],
                          metals_data: {
                            labels: ['Lead', 'Arsenic', 'Mercury', 'Cadmium', 'Zinc', 'Copper'],
                            measured: [0.8, 0.15, 0.003, 0.12, 15.2, 2.8],
                            who_limits: [0.01, 0.01, 0.006, 0.003, 3.0, 2.0]
                          }
                        },
                        '2011': {
                          title: '2011 Cerro Rico Analysis',
                          description: 'Extreme acid mine drainage documentation with pH as low as 0.9',
                          stations: 16,
                          metals: 12,
                          critical: 8,
                          exceeds: '94%',
                          riskLevel: 'critical',
                          coordinates: [
                            {lat: -19.5623, lng: -65.7650, name: 'Cerro Rico Portal 1', risk: 'critical'},
                            {lat: -19.5723, lng: -65.7750, name: 'Pailaviri Tailings', risk: 'critical'},
                            {lat: -19.5523, lng: -65.7550, name: 'Abandoned Portal 2A', risk: 'high'}
                          ],
                          metals_data: {
                            labels: ['Iron', 'Aluminum', 'Zinc', 'Copper', 'Lead', 'Manganese'],
                            measured: [72100, 7480, 19600, 310, 34.8, 402],
                            who_limits: [0.3, 0.2, 3.0, 2.0, 0.01, 0.4]
                          }
                        },
                        '2006': {
                          title: '2006 Baseline Study',
                          description: 'Multi-matrix community contamination assessment across 12 Pilcomayo basin communities',
                          stations: 12,
                          metals: 8,
                          critical: 5,
                          exceeds: '75%',
                          riskLevel: 'high',
                          coordinates: [
                            {lat: -19.5423, lng: -65.7450, name: 'Community Station 1', risk: 'high'},
                            {lat: -19.5523, lng: -65.7350, name: 'Agricultural Area', risk: 'moderate'},
                            {lat: -19.5323, lng: -65.7250, name: 'Downstream Sampling', risk: 'moderate'}
                          ],
                          metals_data: {
                            labels: ['Lead', 'Mercury', 'Cadmium', 'Arsenic', 'Zinc', 'Copper'],
                            measured: [0.12, 0.003, 0.008, 0.05, 8.5, 1.2],
                            who_limits: [0.01, 0.006, 0.003, 0.01, 3.0, 2.0]
                          }
                        }
                      };
                      
                      // Initialize map
                      let map;
                      let markers = [];
                      
                      function initMap() {
                        map = L.map('map').setView([-19.5723, -65.7550], 12);
                        L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
                          attribution: '© OpenStreetMap contributors'
                        }).addTo(map);
                      }
                      
                      function updateMap(period) {
                        // Clear existing markers
                        markers.forEach(marker => map.removeLayer(marker));
                        markers = [];
                        
                        const data = studyData[period];
                        if (!data) return;
                        
                        // Add new markers
                        data.coordinates.forEach(point => {
                          const color = {
                            'safe': '#28a745',
                            'moderate': '#ffc107', 
                            'high': '#fd7e14',
                            'critical': '#dc3545'
                          }[point.risk];
                          
                          const marker = L.circleMarker([point.lat, point.lng], {
                            radius: 10,
                            fillColor: color,
                            color: '#fff',
                            weight: 2,
                            opacity: 1,
                            fillOpacity: 0.8
                          }).addTo(map);
                          
                          marker.bindPopup(`
                                           <strong>${point.name}</strong><br>
                                             Risk Level: ${point.risk}<br>
                                             Period: ${period}
                                           `);
                          
                          markers.push(marker);
                        });
                      }
                      
                      // Initialize charts
                      let metalChart, standardsChart;
                      
                      function initCharts() {
                        // Metal concentrations chart
                        const metalCtx = document.getElementById('metalChart').getContext('2d');
                        metalChart = new Chart(metalCtx, {
                          type: 'bar',
                          data: {
                            labels: [],
                            datasets: [{
                              label: 'Measured Concentration',
                              data: [],
                              backgroundColor: 'rgba(102, 126, 234, 0.8)',
                              borderColor: 'rgba(102, 126, 234, 1)',
                              borderWidth: 2
                            }, {
                              label: 'WHO Limit',
                              data: [],
                              backgroundColor: 'rgba(220, 53, 69, 0.3)',
                              borderColor: 'rgba(220, 53, 69, 1)',
                              borderWidth: 2,
                              type: 'line'
                            }]
                          },
                          options: {
                            responsive: true,
                            maintainAspectRatio: false,
                            scales: {
                              y: {
                                type: 'logarithmic',
                                beginAtZero: false
                              }
                            },
                            plugins: {
                              legend: {
                                position: 'bottom'
                              }
                            }
                          }
                        });
                        
                        // Standards exceedance chart
                        const standardsCtx = document.getElementById('standardsChart').getContext('2d');
                        standardsChart = new Chart(standardsCtx, {
                          type: 'doughnut',
                          data: {
                            labels: ['Compliant', 'Minor Exceedance', 'Major Exceedance', 'Critical Exceedance'],
                            datasets: [{
                              data: [25, 30, 25, 20],
                              backgroundColor: [
                                '#28a745',
                                '#ffc107', 
                                '#fd7e14',
                                '#dc3545'
                              ],
                              borderWidth: 2
                            }]
                          },
                          options: {
                            responsive: true,
                            maintainAspectRatio: false,
                            plugins: {
                              legend: {
                                position: 'bottom'
                              }
                            }
                          }
                        });
                      }
                      
                      function updateCharts(period) {
                        const data = studyData[period];
                        if (!data) return;
                        
                        // Update metal chart
                        metalChart.data.labels = data.metals_data.labels;
                        metalChart.data.datasets[0].data = data.metals_data.measured;
                        metalChart.data.datasets[1].data = data.metals_data.who_limits;
                        metalChart.update();
                        
                        // Update standards chart based on period
                        const exceedanceData = {
                          '2024': [33, 30, 22, 15],
                          '2011': [6, 15, 25, 54],
                          '2006': [25, 35, 25, 15]
                        };
                        
                        standardsChart.data.datasets[0].data = exceedanceData[period] || [25, 30, 25, 20];
                        standardsChart.update();
                      }
                      
                      function updateTimelineInfo(period) {
                        const data = studyData[period];
                        if (!data) return;
                        
                        document.getElementById('timeline-info').innerHTML = `
                        <div class="timeline-period">${data.title}</div>
                          <div class="timeline-description">${data.description}</div>
                            <div class="data-summary">
                              <div class="summary-item">
                                <div class="summary-number">${data.stations}</div>
                                  <div class="summary-label">Stations</div>
                                    </div>
                                    <div class="summary-item">
                                      <div class="summary-number">${data.metals}</div>
                                        <div class="summary-label">Heavy Metals</div>
                                          </div>
                                          <div class="summary-item">
                                            <div class="summary-number">${data.critical}</div>
                                              <div class="summary-label">Critical Sites</div>
                                                </div>
                                                <div class="summary-item">
                                                  <div class="summary-number">${data.exceeds}</div>
                                                    <div class="summary-label">WHO Exceeds</div>
                                                      </div>
                                                      </div>
                                                      `;
                      }
                      
                      // Event listeners
                      document.addEventListener('DOMContentLoaded', function() {
                        initMap();
                        initCharts();
                        
                        // Set initial period
                        updateMap('2024');
                        updateCharts('2024');
                        updateTimelineInfo('2024');
                        
                        // Timeline navigation
                        document.querySelectorAll('.timeline-btn').forEach(btn => {
                          btn.addEventListener('click', function() {
                            // Update active button
                            document.querySelectorAll('.timeline-btn').forEach(b => b.classList.remove('active'));
                            this.classList.add('active');
                            
                            const period = this.dataset.period;
                            
                            if (period === 'all') {
                              // Show all periods overlay
                              updateAllPeriods();
                            } else {
                              updateMap(period);
                              updateCharts(period);
                              updateTimelineInfo(period);
                            }
                          });
                        });
                      });
                      
                      function updateAllPeriods() {
                        // Clear existing markers
                        markers.forEach(marker => map.removeLayer(marker));
                        markers = [];
                        
                        // Add markers from all periods with different symbols
                        Object.keys(studyData).forEach(period => {
                          const data = studyData[period];
                          const symbol = {
                            '2024': '●',
                            '2011': '▲', 
                            '2006': '■'
                          }[period];
                          
                          data.coordinates.forEach(point => {
                            const color = {
                              'safe': '#28a745',
                              'moderate': '#ffc107',
                              'high': '#fd7e14', 
                              'critical': '#dc3545'
                            }[point.risk];
                            
                            const marker = L.marker([point.lat, point.lng], {
                              icon: L.divIcon({
                                html: `<div style="color: ${color}; font-size: 20px; text-align: center;">${symbol}</div>`,
                                className: 'custom-marker',
                                iconSize: [20, 20]
                              })
                            }).addTo(map);
                            
                            marker.bindPopup(`
                                             <strong>${point.name}</strong><br>
                                               Risk Level: ${point.risk}<br>
                                               Period: ${period}<br>
                                               Study: ${data.title}
                                             `);
                            
                            markers.push(marker);
                          });
                        });
                        
                        // Update timeline info for all periods
                        document.getElementById('timeline-info').innerHTML = `
                        <div class="timeline-period">All Periods Overview (2006-2024)</div>
                          <div class="timeline-description">Comprehensive 18-year analysis showing temporal contamination patterns across the Pilcomayo Basin</div>
                          <div class="data-summary">
                          <div class="summary-item">
                          <div class="summary-number">43</div>
                          <div class="summary-label">Total Stations</div>
                          </div>
                          <div class="summary-item">
                          <div class="summary-number">25</div>
                          <div class="summary-label">Max Metals</div>
                          </div>
                          <div class="summary-item">
                          <div class="summary-number">16</div>
                          <div class="summary-label">Critical Sites</div>
                          </div>
                          <div class="summary-item">
                          <div class="summary-number">18</div>
                          <div class="summary-label">Years Span</div>
                          </div>
                          </div>
                          `;
                      }
                      </script>
                        </body>
                        </html>