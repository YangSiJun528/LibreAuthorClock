const languages = ['en', 'ko'];
let currentLanguage;
let allQuotes = {};
let allMessages = {};

async function loadQuotes(language) {
    if (allQuotes[language]) return;
    try {
        const response = await fetch(`i18n/${language}/quote.dsv`);
        if (!response.ok) {
            throw new Error(`Could not find quotes file for ${language}`);
        }
        const text = await response.text();
        const lines = text.split('\n');
        allQuotes[language] = {};
        for (const line of lines) {
            if (!line) continue;
            const [timeKey, timeDisplay, quoteText, quoteSource, quoteAuthor] = line.split('|=|');
            if (!allQuotes[language][timeKey]) {
                allQuotes[language][timeKey] = [];
            }
            allQuotes[language][timeKey].push({
                timeDisplay: timeDisplay,
                quoteText: quoteText,
                quoteSource: quoteSource,
                quoteAuthor: quoteAuthor
            });
        }
    } catch (error) {
        console.error(`Failed to load quotes for ${language}:`, error);
        allQuotes[language] = {};
    }
}

async function loadMessages(language) {
    if (allMessages[language]) return;
    try {
        const response = await fetch(`i18n/${language}/system.json`);
        if (response.ok) {
            allMessages[language] = await response.json();
        } else {
            console.warn(`Could not load messages for ${language}`);
            allMessages[language] = {};
        }
    } catch (error) {
        console.error(`Failed to load messages for ${language}:`, error);
        allMessages[language] = {};
    }
}

async function setLanguage(language) {
    await Promise.all([
        loadQuotes(language),
        loadMessages(language)
    ]);
    currentLanguage = language;
    document.documentElement.lang = language;
    document.getElementById('lang-selector').value = language;
    await updateDisplay();
}

function getQuote(timeKey) {
    const quotesForLanguage = allQuotes[currentLanguage];
    if (!quotesForLanguage || !quotesForLanguage[timeKey]) {
        console.error(`No quotes found for ${currentLanguage} at ${timeKey}`);
        return null;
    }
    const quotesForTime = quotesForLanguage[timeKey];
    const randomIndex = Math.floor(Math.random() * quotesForTime.length);
    return quotesForTime[randomIndex];
}

function getMessage(key) {
    if (allMessages[currentLanguage] && allMessages[currentLanguage][key]) {
        return allMessages[currentLanguage][key];
    }
    console.warn(`Message '${key}' not found for ${currentLanguage}`);
    return "";
}

async function updateDisplay() {
    if (!currentLanguage || !allQuotes[currentLanguage] || !allMessages[currentLanguage]) {
        console.error('Data not loaded for', currentLanguage);
        document.getElementById('quote').textContent = getMessage('QUOTE_LOADING')
        document.getElementById('title').textContent = '';
        document.getElementById('author').textContent = '';
        return;
    }
    const now = new Date();
    const timeKey = now.toTimeString().slice(0, 5);
    const quoteData = getQuote(timeKey);
    if (quoteData) {
        const { timeDisplay, quoteText, quoteSource, quoteAuthor } = quoteData;
        const highlightedQuote = quoteText.replace(
            new RegExp(timeDisplay.replace(/[.*+?^${}()|[\]\\]/g, '\\$&'), 'gi'),
            match => `<strong>${match}</strong>`
        );
        document.getElementById('quote').innerHTML = highlightedQuote;
        document.getElementById('title').textContent = quoteSource;
        document.getElementById('author').textContent = quoteAuthor;
    } else {
        document.getElementById('quote').textContent = getMessage('NO_QUOTE_FOUND');
        document.getElementById('title').textContent = '';
        document.getElementById('author').textContent = '';
    }
}

async function startApp() {
    const langSelector = document.getElementById('lang-selector');
    langSelector.addEventListener('change', (event) => {
        setLanguage(event.target.value);
    });
    //TODO: initial language to be users locale
    const initialLanguage = 'en';
    await setLanguage(initialLanguage);
    function scheduleNextUpdate() {
        const now = new Date();
        const seconds = now.getSeconds();
        const milliseconds = now.getMilliseconds();
        const delay = (60 - seconds) * 1000 - milliseconds;
        setTimeout(async () => {
            await updateDisplay();
            scheduleNextUpdate();
        }, delay);
    }
    scheduleNextUpdate();
}

startApp();
