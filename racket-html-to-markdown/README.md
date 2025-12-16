# Racket HTML to Markdown Converter

A Racket implementation of a tool to convert web pages to Markdown format, with special optimization for Racket Rhombus documentation.

## Features

- **Web Scraping**: Fetch web pages from any URL
- **Smart HTML Parsing**: Extract main content while ignoring navigation and ads
- **Custom Conversion**: Special handling for Racket/Rhombus documentation elements
- **Code Block Preservation**: Preserve code blocks with proper syntax highlighting markers
- **Link Handling**: Convert relative links to absolute URLs
- **Image Handling**: Preserve images with proper Markdown syntax
- **Table Conversion**: Convert HTML tables to Markdown tables
- **Progress Tracking**: Visual feedback during conversion process

## Requirements

- Racket 8.0 or higher
- Required packages:
  - `html-parsing`
  - `sxml`

## Installation

### Install Required Packages

```bash
raco pkg install html-parsing sxml
```

### Clone the Repository

```bash
git clone https://github.com/yourusername/racket-html-to-markdown.git
cd racket-html-to-markdown
```

## Usage

### Basic Usage

Convert a single URL to Markdown:

```bash
racket main.rkt https://docs.racket-lang.org/rhombus/index.html
```

### Custom Output File

Specify a custom output file:

```bash
racket main.rkt https://docs.racket-lang.org/rhombus/index.html -o rhombus_docs.md
```

### Custom Output Directory

Specify a custom output directory:

```bash
racket main.rkt https://docs.racket-lang.org/rhombus/index.html -d docs
```

### Verbose Mode

Enable verbose logging:

```bash
racket main.rkt https://docs.racket-lang.org/rhombus/index.html -v
```

## Project Structure

```
racket-html-to-markdown/
├── main.rkt              # Main program
├── url-fetcher.rkt       # URL fetching module
├── html-parser.rkt       # HTML parsing module
├── html-to-markdown.rkt  # HTML to Markdown conversion module
├── file-saver.rkt        # File saving module
└── README.md             # Project documentation
```

## Modules

### 1. `main.rkt`

The main program that coordinates all other modules. It parses command line arguments, fetches the URL, parses the HTML, converts it to Markdown, and saves the result.

### 2. `url-fetcher.rkt`

Handles fetching web content from URLs. It supports both HTTP and HTTPS, sets appropriate headers, and handles HTTP errors.

### 3. `html-parser.rkt`

Parses HTML content into an SXML structure and extracts the main content while removing unwanted elements like scripts, styles, and navigation.

### 4. `html-to-markdown.rkt`

Converts the SXML structure to Markdown format. It handles various HTML elements like headings, paragraphs, lists, links, images, code blocks, tables, and more.

### 5. `file-saver.rkt`

Saves the converted Markdown content to a file, creating directories as needed.

## Example

Convert the Rhombus documentation homepage:

```bash
racket main.rkt https://docs.racket-lang.org/rhombus/index.html -o rhombus_index.md -v
```

This will:
1. Fetch the Rhombus documentation homepage
2. Parse the HTML content
3. Extract the main content
4. Convert it to Markdown
5. Save the result to `rhombus_index.md` in the current directory

## Limitations

- The tool may not handle all HTML edge cases perfectly
- Complex JavaScript-rendered content may not be parsed correctly
- Some CSS-styled elements may not be converted properly

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

- [Racket](https://racket-lang.org/) - The Racket programming language
- [html-parsing](https://docs.racket-lang.org/html-parsing/) - Racket library for parsing HTML
- [sxml](https://docs.racket-lang.org/sxml/) - Racket library for processing XML/HTML data
