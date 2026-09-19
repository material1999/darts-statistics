import os
import pandas as pd

# Folders
input_folder = "input"
output_folder = "output"
player_bio_file = f"{input_folder}/player_bio.csv"

# Create output folder if it doesn't exist
os.makedirs(output_folder, exist_ok=True)

# Load player data once
players = pd.read_csv(player_bio_file, sep=";")

# Create player name -> player_id mapping
player_id_map = dict(
    zip(players["player"], players["player_id"])
)


def get_player_id(name):
    name = str(name).strip()

    if name not in player_id_map:
        raise ValueError(
            f"Player not found in player_bio.csv: '{name}'"
        )

    return player_id_map[name]


def clean_score(score):
    # Converts "2*" -> 2
    return int(str(score).replace("*", "").strip())


def parse_filename(filename):
    """
    Expected filename format:
        YYYY_MM_DD_roundN.xlsx

    Example:
        2024_01_21_round1.xlsx

    Returns:
        year, month, day, round
    """

    name = os.path.splitext(filename)[0]
    parts = name.split("_")

    if len(parts) != 4:
        raise ValueError(
            f"Unexpected filename format: '{filename}'. "
            "Expected YYYY_MM_DD_roundN.xlsx"
        )

    year = int(parts[0])
    month = int(parts[1])
    day = int(parts[2])

    round_number = parts[3].replace("round", "")
    round_number = int(round_number)

    return year, month, day, round_number


# Process every Excel file in the input folder
for filename in sorted(os.listdir(input_folder)):

    if not filename.lower().endswith(".xlsx"):
        continue

    excel_file = os.path.join(input_folder, filename)

    # Parse date and round from filename
    year, month, day, round_number = parse_filename(filename)

    # Create year-specific output folder
    year_folder = os.path.join(output_folder, str(year))
    os.makedirs(year_folder, exist_ok=True)

    # Output filename: roundN.csv
    output_filename = f"round{round_number}.csv"
    output_file = os.path.join(year_folder, output_filename)

    print(f"Processing {filename}...")

    # Load Excel
    matches = pd.read_excel(excel_file)

    # Normalize phase
    matches["phase"] = matches["Phase"].str.strip().str.lower()

    matches.loc[
        matches["phase"] == "group phase",
        "phase"
    ] = "group"

    # Identify knockout matches
    knockout_mask = matches["phase"] == "knockout phase"
    knockout_indices = matches.index[knockout_mask].tolist()

    # First two knockout matches are always semi-finals
    for index in knockout_indices[:2]:
        matches.loc[index, "phase"] = "semi"

    # Remaining knockout matches
    remaining_indices = knockout_indices[2:]

    for index in remaining_indices:
        score1 = clean_score(matches.loc[index, "Result team 1"])
        score2 = clean_score(matches.loc[index, "Result team 2"])

        # Final is first to 4
        if score1 == 4 or score2 == 4:
            matches.loc[index, "phase"] = "final"
        else:
            matches.loc[index, "phase"] = "bronze"

    # Create cleaned results
    results = pd.DataFrame({
        "year": year,
        "month": month,
        "day": day,
        "round": round_number,

        "phase": matches["phase"],

        "player1_id": matches["Team 1"].apply(get_player_id),
        "player1_score": matches["Result team 1"].apply(clean_score),

        "player2_id": matches["Team 2"].apply(get_player_id),
        "player2_score": matches["Result team 2"].apply(clean_score),
    })

    # Make sure bronze comes before final
    bronze_index = results.index[
        results["phase"] == "bronze"
    ].tolist()

    final_index = results.index[
        results["phase"] == "final"
    ].tolist()

    if bronze_index and final_index and bronze_index[0] > final_index[0]:
        results.iloc[[bronze_index[0], final_index[0]]] = (
            results.iloc[[final_index[0], bronze_index[0]]].values
        )

    # Save
    results.to_csv(
        output_file,
        sep=";",
        index=False
    )

    print(f"  Saved {len(results)} matches to {year}/{output_filename}")

print("Done!")