#!/usr/bin/env python3
"""
factsheet/render_previews.py
Renders high-res PNG previews of Page 1, Page 2, and Page 3 of the 3-page factsheet.
"""
import os, subprocess, re

ROOT = os.path.dirname(os.path.abspath(__file__))
html_path = os.path.join(ROOT, "Ultimate_Alpha_Master_3Page_Factsheet.html")
html_content = open(html_path).read()

# Extract the style from head
style_match = re.search(r"(<style>.*?</style>)", html_content, re.DOTALL)
style = style_match.group(1) if style_match else ""

# Split by <div class="page">
raw_pages = html_content.split('<div class="page">')[1:]

chrome = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
ART_DIR = "/Users/cmejo/.gemini/antigravity/brain/64019830-0cac-4bb1-904e-d4560e71468c"

for i in range(1, 4):
    page_content = raw_pages[i-1]
    # Remove trailing </div> if present before the next comment or </body>
    if i < 3:
        page_body = page_content.split('</div>\n\n<!--')[0].strip()
    else:
        page_body = page_content.split('</div>\n\n</body>')[0].strip()
        
    single_html = f"""<!DOCTYPE html><html><head><meta charset="utf-8">{style}
    <style>
      body {{ margin: 0; background: #ffffff; }}
      .page {{ page-break-after: auto !important; height: 11in !important; width: 8.5in !important; }}
    </style></head><body>
    <div class="page">
    {page_body}
    </div>
    </body></html>"""
    
    page_html_path = os.path.join(ROOT, f"temp_page{i}.html")
    with open(page_html_path, "w") as f:
        f.write(single_html)
        
    out_png = os.path.join(ROOT, f"page{i}_preview.png")
    cmd = [
        chrome,
        "--headless=new",
        "--disable-gpu",
        f"--screenshot={out_png}",
        "--window-size=816,1056",
        f"file://{page_html_path}"
    ]
    subprocess.run(cmd, check=True, capture_output=True)
    print(f"Rendered: {out_png}")
    
    art_png = os.path.join(ART_DIR, f"page{i}_preview.png")
    subprocess.run(["cp", out_png, art_png], check=True)

print("All 3 page previews rendered successfully.")
