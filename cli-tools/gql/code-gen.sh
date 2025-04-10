chmod +x ./morpheus
./morpheus build ./
queryFilename=../../lib/mobility-core/auto-generated/Kernel/External/MultiModal/OpenTripPlanner/Query.hs
schemaFilename=../../lib/mobility-core/auto-generated/Kernel/External/MultiModal/OpenTripPlanner/Schema.hs
sed -i 's/\bFloat\b/Double/g' queryFilename
sed -i 's/\bFloat\b/Double/g' schemaFilename