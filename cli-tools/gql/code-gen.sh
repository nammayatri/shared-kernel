chmod +x ./morpheus
./morpheus build ./
queryFilename=../../lib/mobility-core/auto-generated/Kernel/External/MultiModal/OpenTripPlanner/Query-2.hs
schemaFilename=../../lib/mobility-core/auto-generated/Kernel/External/MultiModal/OpenTripPlanner/Schema.hs
if [[ "$OSTYPE" == "darwin"* ]]; then
  # macOS
  sed -i '' 's/[[:<:]]Float[[:>:]]/Double/g' "$queryFilename"
  sed -i '' 's/[[:<:]]Float[[:>:]]/Double/g' "$schemaFilename"
else
  # Linux and other Unix-like
  sed -i 's/\bFloat\b/Double/g' "$queryFilename"
  sed -i 's/\bFloat\b/Double/g' "$schemaFilename"
fi
