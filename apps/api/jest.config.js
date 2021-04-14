/* eslint-disable no-undef */
module.exports = {
   preset: 'ts-jest',
   testEnvironment: 'node',
   modulePathIgnorePatterns: ['dist'],
   setupFilesAfterEnv: ['./__test__/jest.setup.ts'],
   collectCoverage: true,
}
