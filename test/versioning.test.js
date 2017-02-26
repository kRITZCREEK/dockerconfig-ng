"use strict";

const dockerconfig = require("./../index.js");
const assert = require("assert");


describe("config versioning", () => {

    afterEach(() => delete process.env["NODE_CONFIG_CONFIGVERSION"]);

    context("no config version given", () => {
        const config = dockerconfig.getConfig({a: "b"});

        it("simply parses the config", () =>
            assert.deepEqual(config, {a: "b"})
        );
    });

    context("equal versions", () => {
        let config;

        before(() => {
            process.env["NODE_CONFIG_CONFIGVERSION"] = 1481276501;
            config = dockerconfig.getConfig({configVersion: 1481276501, b: "c"});
        });

        it("simply parses the config", () =>
            assert.deepEqual(config, {configVersion: 1481276501, b: "c"})
        );
    });

    context("equal versions, ENV is a string", () => {
        let config;

        before(() => {
            process.env["NODE_CONFIG_CONFIGVERSION"] = "1481276501";
            config = dockerconfig.getConfig({configVersion: 1481276501, b: "c"});
        });

        it("simply parses the config", () =>
            assert.deepEqual(config, {configVersion: 1481276501, b: "c"})
        );
    });

    context("config version is not a number", () => {
        before(() => process.env["NODE_CONFIG_CONFIGVERSION"] = 1234);

        it("throws an error", () =>
            assert.throws(
                () => dockerconfig.getConfig({configVersion: "1234", b: "c"}),
                /configVersion must be a number/
            )
        );
    });

    context("ENV version is not a number", () => {
        before(() => process.env["NODE_CONFIG_CONFIGVERSION"] = "asd");

        it("throws an error", () =>
           assert.throws(
               () => dockerconfig.getConfig({configVersion: 1234, b: "c"}),
                   /NODE_CONFIG_CONFIGVERSION must be a number/
           )
          );
    });

    context("config version given, but no ENV version", () => {
        before(() => delete process.env["NODE_CONFIG_CONFIGVERSION"]);

        it("throws an error", () =>
            assert.throws(
                () => dockerconfig.getConfig({configVersion: 1481276501, b: "c"}),
                /no NODE_CONFIG_CONFIGVERSION given/
            )
        );
    });

    context("config version NOT given, but ENV version given", () => {
        before(() => process.env["NODE_CONFIG_CONFIGVERSION"] = 32);

        it("throws an error", () =>
            assert.throws(
                () => dockerconfig.getConfig({b: "c"}),
                /config has no version, but NODE_CONFIG_CONFIGVERSION given/
            )
        );
    });


    context("outdated ENV version", () => {
        before(() => process.env["NODE_CONFIG_CONFIGVERSION"] = 42);

        it("throws an error", () =>
            assert.throws(
                () => dockerconfig.getConfig({configVersion: 1481276501, b: "c"}),
                /NODE_CONFIG_CONFIGVERSION outdated/
            )
        );
    });

    context("outdated config version", () => {
        before(() => process.env["NODE_CONFIG_CONFIGVERSION"] = 1481276502);

        it("throws an error", () =>
            assert.throws(
                () => dockerconfig.getConfig({configVersion: 1481276501, b: "c"}),
                /NODE_CONFIG_CONFIGVERSION newer than config/
            )
        );
    });

    context("overlapping properties", () => {
        it("detects and errors on overlapping properties", () =>
           assert.throws(
               () => dockerconfig.getConfig({value: 1481276501, Value: "c"}),
                   /Overlapping properties/));
    })
});
