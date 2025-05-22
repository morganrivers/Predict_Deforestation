import subprocess
import time
from pathlib import Path

def iso3_list(csv_path: Path) -> list[str]:
    codes = set()
    with open(csv_path, encoding="utf-8") as f:
        next(f)  # skip header
        for line in f:
            line = line.strip()
            if not line or line == "":
                continue
            if line.startswith('"') and line.endswith('"'):
                line = line[1:-1]
            fields = [s.strip() for s in line.split(",")]
            if len(fields) > 11:
                code = fields[11]
                if code and len(code) == 3:
                    codes.add(code)
    return sorted(codes)

def download_cckp_csv(variable, iso, outdir, start=1990, end=2023):
    url = (
        f"https://cckpapi.worldbank.org/cckp/v1/"
        f"era5-x0.25_timeseries_{variable}_timeseries_annual_{start}-{end}_mean_historical_ensemble_all_mean/"
        f"{iso}?_format=csv"
    )
    outfile = Path(outdir) / f"{iso}_{variable}_annual_{start}_{end}.csv"
    cmd = ["curl", "-sSfL", "-o", str(outfile), url]
    print(f"Downloading {variable} {iso}: {url}")
    res = subprocess.run(cmd)
    if res.returncode != 0:
        print(f"  Failed to download {iso} {variable} (exit {res.returncode})")
        outfile.unlink(missing_ok=True)  # clean up partial file if error
    time.sleep(1.2)  # adjust to avoid rate limit

def main():
    data_dir = Path("data")
    iso_codes = iso3_list(data_dir / "2022-09-24__CSV_UNSD_M49.csv")
    variables = ["pr", "cdd", "fwi", "tas", "hdd65", "cdd65"]

    for variable in variables:
        for iso in iso_codes:
            download_cckp_csv(variable, iso, data_dir)

if __name__ == "__main__":
    main()
