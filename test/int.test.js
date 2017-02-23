var dockerconfig = require("./../index.js");
var conf = require("./testconfig.json");
var assert = require("assert");


describe("dockerconfig", () => {

    var CONFIG;
    before(() => {
        process.env["NODE_CONFIG_DATABASE_POSTGRES_CONSTRING"] = '[{ \"host\": \"123\", \"port\": 123 }]';
        process.env["NODE_CONFIG_ENABLEFEATUREX"] = '1';
        process.env["NODE_CONFIG_WOWDUDE_ENABLEFEATUREX"] = '1';

        CONFIG = dockerconfig.getConfig(conf);
    });

    it("sets the expected values", () =>
        assert.deepEqual(CONFIG.database, { postgres: { constring: [ {host: "123", port: 123} ] } })
    );

    it("sets config values with uppercase letters", () =>
        assert.deepEqual(CONFIG.enableFeatureX, true)
      );
    it("sets config values with nested uppercase letters", () =>
       assert.deepEqual(CONFIG.wowDude.enableFeatureX, true)
      );
});
