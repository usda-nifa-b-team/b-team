/* global imerss, data, svg, width, height, options */

"use strict";

// Driver which passes the R2D3 globals into our JS implementation

debugger;

// Flatten the incoming dataframe into the array of arrays that the JS is expecting
const flatData = data.map(row => [row.Bee, row.Plant, row.Frequency]);

// Convert dataframes within the supplied options
const driverOptions = {
    ...options,
    // R2D3 renders dataframes in options a hash of columns, zip them together
    beeColors: Object.fromEntries(options.beeColors.Taxon.map( (taxon, index) => [taxon, options.beeColors.Color[index]])),
    // This one just consists of one column, simply forward it
    sortedBeeNames: options.sortedBeeNames.Taxon,
    sortedPlantNames: options.sortedPlantNames.Taxon
};

imerss.bipartitePP(flatData, svg, width, height, driverOptions);
