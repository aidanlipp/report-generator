import React, { useState, useRef } from 'react';
import { BarChart, Bar, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer, LineChart, Line, AreaChart, Area } from 'recharts';
import { Upload, Download, FileText, TrendingUp, Users, Target } from 'lucide-react';

const BaseballAnalyzer = () => {
  const [csvData, setCsvData] = useState(null);
  const [selectedPlayer, setSelectedPlayer] = useState('');
  const [playerData, setPlayerData] = useState(null);
  const [chartType, setChartType] = useState('area');
  const [recommendations, setRecommendations] = useState({
    kmotion: '',
    forceplate: '',
    swing: '',
    movement: [
      { test: '', findings: '' }
    ]
  });
  const fileInputRef = useRef(null);

  const handleFileUpload = (event) => {
    const file = event.target.files[0];
    if (file && file.type === 'text/csv') {
      const reader = new FileReader();
      reader.onload = (e) => {
        const text = e.target.result;
        const rows = text.split('\n');
        const headers = rows[0].split(',').map(h => h.trim());
        
        const data = rows.slice(1).filter(row => row.trim()).map(row => {
          const values = row.split(',').map(v => v.trim());
          const obj = {};
          headers.forEach((header, index) => {
            obj[header] = values[index];
          });
          return obj;
        });
        
        setCsvData(data);
        if (data.length > 0) {
          setSelectedPlayer(data[0]['First Name'] + ' ' + data[0]['Last Name']);
        }
      };
      reader.readAsText(file);
    }
  };

  const processPlayerData = (playerName) => {
    if (!csvData) return null;
    
    const player = csvData.find(p => (p['First Name'] + ' ' + p['Last Name']) === playerName);
    if (!player) return null;

    const swings = [];
    const metrics = ['Exit Velo', 'Bat Speed', 'Rot. Acc.', 'VBA', 'Attack Angle', 'On Plane Efficiency'];
    
    for (let i = 1; i <= 5; i++) {
      const swing = {};
      metrics.forEach(metric => {
        const value = player[`${metric} ${i}`];
        if (value && value !== '') {
          swing[metric] = parseFloat(value) || value;
        }
      });
      if (Object.keys(swing).length > 0) {
        swing.swing = i;
        swings.push(swing);
      }
    }

    // Calculate averages
    const averages = {};
    metrics.forEach(metric => {
      const values = swings.map(s => s[metric]).filter(v => v !== undefined && !isNaN(v));
      if (values.length > 0) {
        averages[metric] = values.reduce((a, b) => a + b, 0) / values.length;
      }
    });

    return {
      name: playerName,
      swings,
      averages,
      rawData: player
    };
  };

  React.useEffect(() => {
    if (selectedPlayer) {
      const data = processPlayerData(selectedPlayer);
      setPlayerData(data);
    }
  }, [selectedPlayer, csvData]);

  const getPlayerList = () => {
    if (!csvData) return [];
    return csvData.map(p => p['First Name'] + ' ' + p['Last Name']).filter(Boolean);
  };

  const createDistributionData = (metric) => {
    if (!playerData || !playerData.swings) return [];
    
    const values = playerData.swings.map(s => s[metric]).filter(v => v !== undefined && !isNaN(v));
    if (values.length === 0) return [];

    const min = Math.min(...values);
    const max = Math.max(...values);
    const range = max - min;
    const bins = 10;
    const binSize = range / bins;

    const distribution = [];
    for (let i = 0; i < bins; i++) {
      const binStart = min + i * binSize;
      const binEnd = min + (i + 1) * binSize;
      const count = values.filter(v => v >= binStart && v < binEnd).length;
      distribution.push({
        bin: `${binStart.toFixed(1)}-${binEnd.toFixed(1)}`,
        count,
        density: count / values.length,
        midpoint: (binStart + binEnd) / 2
      });
    }

    return distribution;
  };

  const renderChart = (data, title, color) => {
    if (!data || data.length === 0) return null;

    const chartProps = {
      width: 300,
      height: 200,
      data: data,
      margin: { top: 5, right: 30, left: 20, bottom: 5 }
    };

    switch (chartType) {
      case 'bar':
        return (
          <ResponsiveContainer {...chartProps}>
            <BarChart data={data}>
              <CartesianGrid strokeDasharray="3 3" />
              <XAxis dataKey="bin" fontSize={10} />
              <YAxis fontSize={10} />
              <Tooltip />
              <Bar dataKey="density" fill={color} />
            </BarChart>
          </ResponsiveContainer>
        );
      case 'line':
        return (
          <ResponsiveContainer {...chartProps}>
            <LineChart data={data}>
              <CartesianGrid strokeDasharray="3 3" />
              <XAxis dataKey="bin" fontSize={10} />
              <YAxis fontSize={10} />
              <Tooltip />
              <Line type="monotone" dataKey="density" stroke={color} strokeWidth={2} />
            </LineChart>
          </ResponsiveContainer>
        );
      default: // area
        return (
          <ResponsiveContainer {...chartProps}>
            <AreaChart data={data}>
              <CartesianGrid strokeDasharray="3 3" />
              <XAxis dataKey="bin" fontSize={10} />
              <YAxis fontSize={10} />
              <Tooltip />
              <Area type="monotone" dataKey="density" stroke={color} fill={color} fillOpacity={0.6} />
            </AreaChart>
          </ResponsiveContainer>
        );
    }
  };

  const generateReport = () => {
    if (!playerData) return;

    const reportContent = `
# ${playerData.name} - Performance Analysis & Recommendations

## Performance Metrics
- Exit Velocity: ${playerData.averages['Exit Velo']?.toFixed(1) || 'N/A'} mph
- Bat Speed: ${playerData.averages['Bat Speed']?.toFixed(1) || 'N/A'} mph
- Rotational Acceleration: ${playerData.averages['Rot. Acc.']?.toFixed(1) || 'N/A'} deg/s²
- VBA: ${playerData.averages['VBA']?.toFixed(1) || 'N/A'}°
- Attack Angle: ${playerData.averages['Attack Angle']?.toFixed(1) || 'N/A'}°
- On Plane Efficiency: ${playerData.averages['On Plane Efficiency']?.toFixed(1) || 'N/A'}%

## K-Motion Analysis
${recommendations.kmotion}

## Force Plate Analysis
${recommendations.forceplate}

## Swing Recommendations
${recommendations.swing}

## Movement Screen Assessment
${recommendations.movement.map(item => `**${item.test}**: ${item.findings}`).join('\n')}
    `;

    const blob = new Blob([reportContent], { type: 'text/markdown' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = `${playerData.name}_Performance_Report.md`;
    a.click();
    URL.revokeObjectURL(url);
  };

  return (
    <div className="max-w-6xl mx-auto p-6 bg-white">
      <div className="mb-8">
        <h1 className="text-3xl font-bold text-gray-800 mb-2 flex items-center gap-2">
          <Target className="w-8 h-8 text-blue-600" />
          Baseball Performance Analysis Tool
        </h1>
        <p className="text-gray-600">Upload CSV data and create professional performance reports</p>
      </div>

      {/* File Upload Section */}
      <div className="mb-8 p-6 border-2 border-dashed border-gray-300 rounded-lg">
        <div className="text-center">
          <Upload className="w-12 h-12 text-gray-400 mx-auto mb-4" />
          <h3 className="text-lg font-semibold mb-2">Upload CSV Data</h3>
          <p className="text-gray-600 mb-4">Upload your baseball performance CSV file</p>
          <input
            type="file"
            accept=".csv"
            onChange={handleFileUpload}
            ref={fileInputRef}
            className="hidden"
          />
          <button
            onClick={() => fileInputRef.current?.click()}
            className="bg-blue-600 text-white px-6 py-2 rounded-lg hover:bg-blue-700 transition-colors"
          >
            Choose File
          </button>
          {csvData && (
            <p className="text-green-600 mt-2">✓ CSV loaded with {csvData.length} players</p>
          )}
        </div>
      </div>

      {csvData && (
        <>
          {/* Player Selection */}
          <div className="mb-6">
            <label className="block text-sm font-medium text-gray-700 mb-2">
              Select Player
            </label>
            <select
              value={selectedPlayer}
              onChange={(e) => setSelectedPlayer(e.target.value)}
              className="w-full p-3 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-blue-500"
            >
              {getPlayerList().map(player => (
                <option key={player} value={player}>{player}</option>
              ))}
            </select>
          </div>

          {/* Chart Type Selection */}
          <div className="mb-6">
            <label className="block text-sm font-medium text-gray-700 mb-2">
              Chart Type
            </label>
            <div className="flex gap-4">
              <button
                onClick={() => setChartType('area')}
                className={`px-4 py-2 rounded-lg ${chartType === 'area' ? 'bg-blue-600 text-white' : 'bg-gray-200 text-gray-700'}`}
              >
                Area Chart
              </button>
              <button
                onClick={() => setChartType('bar')}
                className={`px-4 py-2 rounded-lg ${chartType === 'bar' ? 'bg-blue-600 text-white' : 'bg-gray-200 text-gray-700'}`}
              >
                Bar Chart
              </button>
              <button
                onClick={() => setChartType('line')}
                className={`px-4 py-2 rounded-lg ${chartType === 'line' ? 'bg-blue-600 text-white' : 'bg-gray-200 text-gray-700'}`}
              >
                Line Chart
              </button>
            </div>
          </div>

          {playerData && (
            <>
              {/* Performance Analysis */}
              <div className="mb-8">
                <h2 className="text-2xl font-bold text-gray-800 mb-4">
                  {playerData.name} - Performance Analysis
                </h2>
                
                {/* Distribution Charts */}
                <div className="grid grid-cols-1 md:grid-cols-2 gap-6 mb-8">
                  <div className="bg-gray-50 p-4 rounded-lg">
                    <h3 className="text-lg font-semibold mb-2">Bat Speed Distribution</h3>
                    {renderChart(createDistributionData('Bat Speed'), 'Bat Speed', '#3B82F6')}
                  </div>
                  <div className="bg-gray-50 p-4 rounded-lg">
                    <h3 className="text-lg font-semibold mb-2">Rotational Acceleration Distribution</h3>
                    {renderChart(createDistributionData('Rot. Acc.'), 'Rotational Acceleration', '#EF4444')}
                  </div>
                </div>

                {/* Performance Metrics Table */}
                <div className="bg-white border rounded-lg overflow-hidden mb-8">
                  <table className="w-full">
                    <thead className="bg-green-600 text-white">
                      <tr>
                        <th className="px-6 py-3 text-left font-semibold">Metric</th>
                        <th className="px-6 py-3 text-left font-semibold">Player Avg</th>
                        <th className="px-6 py-3 text-left font-semibold">Notes</th>
                      </tr>
                    </thead>
                    <tbody>
                      <tr className="border-b bg-green-50">
                        <td className="px-6 py-3 font-medium">Exit Velocity</td>
                        <td className="px-6 py-3">{playerData.averages['Exit Velo']?.toFixed(1) || 'N/A'}</td>
                        <td className="px-6 py-3 text-sm text-gray-600">mph</td>
                      </tr>
                      <tr className="border-b bg-red-50">
                        <td className="px-6 py-3 font-medium">Bat Speed</td>
                        <td className="px-6 py-3">{playerData.averages['Bat Speed']?.toFixed(1) || 'N/A'}</td>
                        <td className="px-6 py-3 text-sm text-gray-600">mph</td>
                      </tr>
                      <tr className="border-b bg-red-50">
                        <td className="px-6 py-3 font-medium">Rotational Acceleration</td>
                        <td className="px-6 py-3">{playerData.averages['Rot. Acc.']?.toFixed(1) || 'N/A'}</td>
                        <td className="px-6 py-3 text-sm text-gray-600">deg/s²</td>
                      </tr>
                      <tr className="border-b bg-red-50">
                        <td className="px-6 py-3 font-medium">VBA</td>
                        <td className="px-6 py-3">{playerData.averages['VBA']?.toFixed(1) || 'N/A'}</td>
                        <td className="px-6 py-3 text-sm text-gray-600">degrees</td>
                      </tr>
                      <tr className="border-b bg-green-50">
                        <td className="px-6 py-3 font-medium">Attack Angle</td>
                        <td className="px-6 py-3">{playerData.averages['Attack Angle']?.toFixed(1) || 'N/A'}</td>
                        <td className="px-6 py-3 text-sm text-gray-600">degrees</td>
                      </tr>
                      <tr className="border-b bg-green-50">
                        <td className="px-6 py-3 font-medium">On Plane Efficiency</td>
                        <td className="px-6 py-3">{playerData.averages['On Plane Efficiency']?.toFixed(1) || 'N/A'}%</td>
                        <td className="px-6 py-3 text-sm text-gray-600">percentage</td>
                      </tr>
                    </tbody>
                  </table>
                </div>
              </div>

              {/* Recommendations Input */}
              <div className="mb-8">
                <h2 className="text-2xl font-bold text-gray-800 mb-4 flex items-center gap-2">
                  <FileText className="w-6 h-6" />
                  Analysis & Recommendations
                </h2>
                
                <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                  <div>
                    <label className="block text-sm font-medium text-gray-700 mb-2">
                      K-Motion Analysis
                    </label>
                    <textarea
                      value={recommendations.kmotion}
                      onChange={(e) => setRecommendations(prev => ({ ...prev, kmotion: e.target.value }))}
                      className="w-full p-3 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-blue-500"
                      rows="4"
                      placeholder="Enter K-Motion analysis findings..."
                    />
                  </div>
                  
                  <div>
                    <label className="block text-sm font-medium text-gray-700 mb-2">
                      Force Plate Analysis
                    </label>
                    <textarea
                      value={recommendations.forceplate}
                      onChange={(e) => setRecommendations(prev => ({ ...prev, forceplate: e.target.value }))}
                      className="w-full p-3 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-blue-500"
                      rows="4"
                      placeholder="Enter Force Plate analysis findings..."
                    />
                  </div>
                  
                  <div className="md:col-span-2">
                    <label className="block text-sm font-medium text-gray-700 mb-2">
                      Swing Recommendations
                    </label>
                    <textarea
                      value={recommendations.swing}
                      onChange={(e) => setRecommendations(prev => ({ ...prev, swing: e.target.value }))}
                      className="w-full p-3 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-blue-500"
                      rows="4"
                      placeholder="Enter swing recommendations..."
                    />
                  </div>
                </div>
              </div>

              {/* Movement Screen Assessment */}
              <div className="mb-8">
                <h3 className="text-xl font-semibold text-gray-800 mb-4 flex items-center justify-between">
                  Movement Screen Assessment
                  <button
                    onClick={() => setRecommendations(prev => ({
                      ...prev,
                      movement: [...prev.movement, { test: '', findings: '' }]
                    }))}
                    className="bg-blue-600 text-white px-3 py-1 rounded text-sm hover:bg-blue-700 transition-colors"
                  >
                    Add Test
                  </button>
                </h3>
                <div className="space-y-4">
                  {recommendations.movement.map((item, index) => (
                    <div key={index} className="flex items-start gap-4 p-4 border border-gray-200 rounded-lg">
                      <input
                        type="text"
                        value={item.test}
                        onChange={(e) => {
                          const newMovement = [...recommendations.movement];
                          newMovement[index].test = e.target.value;
                          setRecommendations(prev => ({ ...prev, movement: newMovement }));
                        }}
                        className="w-48 p-2 border border-gray-300 rounded focus:ring-2 focus:ring-blue-500 focus:border-blue-500"
                        placeholder="Test name (e.g., Pelvic Tilt)"
                      />
                      <textarea
                        value={item.findings}
                        onChange={(e) => {
                          const newMovement = [...recommendations.movement];
                          newMovement[index].findings = e.target.value;
                          setRecommendations(prev => ({ ...prev, movement: newMovement }));
                        }}
                        className="flex-1 p-2 border border-gray-300 rounded focus:ring-2 focus:ring-blue-500 focus:border-blue-500"
                        rows="2"
                        placeholder="Enter findings for this test..."
                      />
                      {recommendations.movement.length > 1 && (
                        <button
                          onClick={() => {
                            const newMovement = recommendations.movement.filter((_, i) => i !== index);
                            setRecommendations(prev => ({ ...prev, movement: newMovement }));
                          }}
                          className="bg-red-500 text-white px-3 py-2 rounded hover:bg-red-600 transition-colors"
                        >
                          Remove
                        </button>
                      )}
                    </div>
                  ))}
                </div>
              </div>

              {/* Generate Report Button */}
              <div className="text-center">
                <button
                  onClick={generateReport}
                  className="bg-green-600 text-white px-8 py-3 rounded-lg hover:bg-green-700 transition-colors flex items-center gap-2 mx-auto"
                >
                  <Download className="w-5 h-5" />
                  Generate Report
                </button>
              </div>
            </>
          )}
        </>
      )}
    </div>
  );
};

export default BaseballAnalyzer;
