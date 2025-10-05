# LaTeX compilation configuration for VS Code LaTeX Workshop
# This file configures latexmk for proper compilation with biblatex/biber

# Use pdflatex as the primary compiler
$pdf_mode = 1;
$pdflatex = 'pdflatex -interaction=nonstopmode -synctex=1 %O %S';

# Configure biber for bibliography processing
$biber = 'biber %O %B';
$bibtex_use = 2;  # Use biber instead of bibtex

# Clean up auxiliary files
$clean_ext = 'synctex.gz synctex.gz(busy) run.xml tex.bak bbl bcf fdb_latexmk blg nav snm vrb';

# Maximum runs to resolve references
$max_repeat = 5;

# Continue compilation even if there are errors
$force_mode = 1;

# Show all warnings
$warnings = 1;