import fs from 'fs/promises'
import { csvParse, autoType } from 'd3-dsv'
import { scaleThreshold } from 'd3-scale';
import { feature, mesh } from 'topojson-client';
import { geoPath } from 'd3-geo';
import { map } from 'd3-collection';
import { geoRobinson } from 'd3-geo-projection';
import pkg from 'canvas';
const { createCanvas } = pkg

const generateMap = async (name) => {
  const width = 240;
  const height = 120;

  const raw = await fs.readFile('../../output-data/output-for-interactive/main_map.csv');
  const data = csvParse(raw.toString(), autoType);
  const dataMap = map(data, (d) => d.iso3c);

  const rawWorld = await fs.readFile('./countries_50m.json');
  const world = JSON.parse(rawWorld);

  const countries = feature(world, world.objects.countries);
  const innerlines = mesh(world, world.objects.innerlines);
  const worldMesh = mesh(world, world.objects.world);

  const projection = geoRobinson()
    .scale(0.18 * width)
    .center([4, 0])
    .rotate([-10, 0])
    .translate([width / 2, height / 2 + Math.min(60, width * 0.05)])
    .precision(0.1);

  const z = scaleThreshold()
    .range([
      '#e0eef5',
      '#fef0d9',
      '#fdd49e',
      '#fdbb84',
      '#fc8d59',
      '#ef6548',
      '#d7301f',
      '#990000',
    ])
    .domain([0, 25, 50, 100, 150, 250, 350])
    .unknown('#fff');

  // draw canvas
  const canvas = createCanvas(width, height);
  const ctx = canvas.getContext('2d');

  const path = geoPath(projection).context(ctx);

  ctx.lineJoin = 'round';
  ctx.lineCap = 'round';

  for (let i = 0; i < countries.features.length; i++) {
    const d = countries.features[i];

    // render countries
    ctx.beginPath();
    if (dataMap.get(d.id)) {
      if (dataMap.get(d.id)[name] === 'NA') {
        ctx.fillStyle = 'white';
      } else {
        ctx.fillStyle = z(dataMap.get(d.id)[name]);
      }
    } else {
      ctx.fillStyle = 'white';
    }
    path(d);
    ctx.fill();
    ctx.closePath();
  }

  ctx.beginPath();
  ctx.strokeStyle = '#121212'
  ctx.lineWidth = 0.1;
  path(innerlines);
  ctx.stroke();
  ctx.closePath();

  ctx.beginPath();
  ctx.lineWidth = 0.1;
  ctx.strokeStyle = '#121212'
  path(worldMesh);
  ctx.stroke();
  ctx.closePath();

  const buffer = canvas.toBuffer('image/png')
  await fs.writeFile(`../../output-data/output-for-interactive/${name}.png`, buffer)
}

(async () => {
  await generateMap('cumulative_estimated_daily_excess_deaths_per_100k');
  await generateMap('cumulative_covid_deaths_per_100k');
})();