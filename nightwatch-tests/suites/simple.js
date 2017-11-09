const url = "http://localhost:8000/nightwatch-tests/index.html";

const titleSelector = "h3"

const textInputSelector = ".elm-time-picker-input-container input";

const hourListSelector = ".elm-time-picker-panel-select ul:first-child";
const minuteListSelector = ".elm-time-picker-panel-select ul:first-child";
const secondListSelector = ".elm-time-picker-panel-select ul:first-child";

const hour12Selector = hourListSelector + " li:nth-child(1)";
const hour5Selector = hourListSelector + " li:nth-child(6)";
const hour7Selector = hourListSelector + " li:nth-child(8)";
const hour11Selector = hourListSelector + " li:nth-child(12)";

const selectedHoursSelector = "#selected-hours";
const selectedMinutesSelector = "#selected-minutes";
const selectedSecondsSelector = "#selected-seconds";

const testValidManualInput = (input, expectedResult, hours, minutes, seconds) => {
  return {
    [`It should support manually entering "${input}"`]: (client) => {
      client.url(url);
      client.expect.element(textInputSelector).to.be.present.before(1000);
      client.click(textInputSelector);
      sendSlowly(client,textInputSelector, [...input, client.Keys.ENTER]);

      // now we click on another value, to make sure the input is uptimed
      client.click(titleSelector);
      client.expect.element(textInputSelector).value.to.equal(expectedResult).before(1000);
      client.expect.element(selectedHoursSelector).text.to.equal(hours);
      client.expect.element(selectedMinutesSelector).text.to.equal(minutes);
      client.expect.element(selectedSecondsSelector).text.to.equal(seconds);
      client.end();
    },
  }
}

const testInvalidManualInput = (input) => {
  return {
    [`It should not allow manually entering "${input}"`]: (client) => {
      client.url(url);
      client.expect.element(textInputSelector).to.be.present.before(1000);
      client.click(textInputSelector);
      sendSlowly(client,textInputSelector, [...input, client.Keys.ENTER]);

      // now we click on another value, to make sure the input is uptimed
      client.click(titleSelector);
      client.expect.element(textInputSelector).value.to.equal("").before(1000);
      client.expect.element(selectedHoursSelector).text.to.equal("");
      client.expect.element(selectedMinutesSelector).text.to.equal("");
      client.expect.element(selectedSecondsSelector).text.to.equal("");
      client.end();
    },
  }
}

const clearWithBackspace = (client, selector, n) => {
  for(var i = 0; i < n; i++) {
    client.setValue(selector, client.Keys.BACKSPACE);
    client.pause(1);
  }
}

const sendSlowly = (client, selector, keys) => {
  keys.forEach ((key) => {
    client.setValue(selector, key);
    client.pause(1);
  });
}

module.exports = {
  'When selecting hour 5 with the mouse, it should default the period to PM and appear in the text input': (client) => {
    client.url(url);
    client.expect.element(textInputSelector).to.be.present.before(1000);
    client.click(textInputSelector);
    client.expect.element(hour5Selector).to.be.present.before(1000);
    client.click(hour5Selector);
    client.expect.element(textInputSelector).value.to.equal("5:00:00 PM").before(1000);
    client.expect.element(selectedHoursSelector).text.to.equal(17);
    client.expect.element(selectedMinutesSelector).text.to.equal(0);
    client.expect.element(selectedSecondsSelector).text.to.equal(0);
    client.end();
  },

  'When selecting hour 12 with the mouse, it should default the period to PM and appear in the text input': (client) => {
    client.url(url);
    client.expect.element(textInputSelector).to.be.present.before(1000);
    client.click(textInputSelector);
    client.expect.element(hour12Selector).to.be.present.before(1000);
    client.click(hour12Selector);
    client.expect.element(textInputSelector).value.to.equal("12:00:00 PM").before(1000);
    client.expect.element(selectedHoursSelector).text.to.equal(12);
    client.expect.element(selectedMinutesSelector).text.to.equal(0);
    client.expect.element(selectedSecondsSelector).text.to.equal(0);
    client.end();
  },

  'When entering text, and then selecting a time with the mouse, the selected time should appear in the text input': (client) => {
    client.url(url);
    client.expect.element(textInputSelector).to.be.present.before(1000);
    client.click(textInputSelector);
    client.setValue(textInputSelector, "1:00:00 AM");
    client.expect.element(hour5Selector).to.be.present.before(1000);
    client.click(hour5Selector);
    client.expect.element(textInputSelector).value.to.equal("5:00:00 PM").before(1000);
    client.expect.element(selectedHoursSelector).text.to.equal(17);
    client.expect.element(selectedMinutesSelector).text.to.equal(0);
    client.expect.element(selectedSecondsSelector).text.to.equal(0);
    client.end();
  },

  'When a valid time has been entered, and then selecting an hour with the mouse, the selected hour should use the period from the input time': (client) => {
    client.url(url);
    client.expect.element(textInputSelector).to.be.present.before(1000);
    client.click(textInputSelector);
    sendSlowly(client,textInputSelector, [..."1:00:00 AM", client.Keys.ENTER]);
    client.expect.element(textInputSelector).value.to.equal("1:00:00 AM").before(1000);
    client.expect.element(hour5Selector).to.be.present.before(1000);
    client.click(hour5Selector);
    client.expect.element(textInputSelector).value.to.equal("5:00:00 AM").before(1000);
    client.expect.element(selectedHoursSelector).text.to.equal(5);
    client.expect.element(selectedMinutesSelector).text.to.equal(0);
    client.expect.element(selectedSecondsSelector).text.to.equal(0);
    client.end();
  },

  'When selecting a time, and then typing a valid time, the typed time should appear in the text input': (client) => {
    client.url(url);
    
    client.expect.element(textInputSelector).to.be.present.before(1000);
    client.click(textInputSelector);
    client.expect.element(hour5Selector).to.be.present.before(1000);
    client.click(hour5Selector);
    client.expect.element(textInputSelector).value.to.equal("5:00:00 PM").before(1000);
    
    client.click(textInputSelector);
    client.clearValue(textInputSelector);
    client.pause(100);
    sendSlowly(client,textInputSelector, [..."1:00:00 AM", client.Keys.ENTER]);
    
    client.expect.element(textInputSelector).value.to.equal("1:00:00 AM").before(1000);
    client.expect.element(selectedHoursSelector).text.to.equal(1);
    client.expect.element(selectedMinutesSelector).text.to.equal(0);
    client.expect.element(selectedSecondsSelector).text.to.equal(0);
    
    client.end();
  },

  'When selecting a time, and then typing an invalid time, the selected time should appear in the text input': (client) => {
    client.url(url);
    
    client.expect.element(textInputSelector).to.be.present.before(1000);
    client.click(textInputSelector);
    client.expect.element(hour5Selector).to.be.present.before(1000);
    client.click(hour5Selector);

    const valid_time = "5:00:00 PM"

    client.expect.element(textInputSelector).value.to.equal(valid_time).before(1000);

    client.click(textInputSelector);
    clearWithBackspace(client, textInputSelector, valid_time.length);
    sendSlowly(client,textInputSelector, [..."27:00:00 AM", client.Keys.ENTER]);
    
    client.expect.element(textInputSelector).value.to.equal(valid_time).before(1000);
    client.expect.element(selectedHoursSelector).text.to.equal(17);
    client.expect.element(selectedMinutesSelector).text.to.equal(0);
    client.expect.element(selectedSecondsSelector).text.to.equal(0);
    
    client.end();
  },

  ...testValidManualInput("12:15:50 PM", "12:15:50 PM", 12, 15, 50),
  ...testValidManualInput("11 PM", "11:00:00 PM", 23, 0, 0),
  ...testValidManualInput("11PM", "11:00:00 PM", 23, 0, 0),
  ...testValidManualInput("23 AM", "11:00:00 PM", 23, 0, 0),
  ...testValidManualInput("   9    AM   ", "9:00:00 AM", 9, 0, 0),
  ...testValidManualInput("23:45:00", "11:45:00 PM", 23, 45, 0),
  ...testValidManualInput("12 AM", "12:00:00 AM", 0, 0, 0),
  ...testValidManualInput("0", "12:00:00 AM", 0, 0, 0),
  ...testValidManualInput("0PM", "12:00:00 PM", 12, 0, 0),
  ...testValidManualInput("7", "7:00:00 AM", 7, 0, 0),
  ...testValidManualInput("11", "11:00:00 AM", 11, 0, 0),
  ...testValidManualInput("12", "12:00:00 PM", 12, 0, 0),
  ...testValidManualInput("1", "1:00:00 PM", 13, 0, 0),
  ...testValidManualInput("6", "6:00:00 PM", 18, 0, 0),

  ...testInvalidManualInput("blarg"),
  ...testInvalidManualInput("b 12:00:00 PM b"),
  ...testInvalidManualInput("-1:00:00 AM"),
  ...testInvalidManualInput("25:00:00 PM"),
  ...testInvalidManualInput("12:60:00 PM"),
  ...testInvalidManualInput("12:-1:00 PM"),
  ...testInvalidManualInput("12:00:60 PM"),
  ...testInvalidManualInput("12:00:-1 PM"),
};