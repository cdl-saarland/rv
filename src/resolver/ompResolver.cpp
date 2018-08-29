

class OpenMPResolver : public ResolverService {
};

void
OpenMPResolver::registerDeclareSIMDFunction(Function & F) {
  auto attribSet = F.getAttributes().getFnAttributes();
  // parse SIMD signatures
  std::vector<VectorMapping> wfvJobs;
  for (auto attrib : attribSet) {
    if (!attrib.isStringAttribute()) continue;
    StringRef attribText = attrib.getKindAsString();

    if (attribText.size() < 2) continue;

    VectorMapping vecMapping;
    if (!parseVectorMapping(F, attribText, vecMapping, false)) continue;
    addMapping(vecMapping);
  }
}

void addOpenMPResolver(const Config & config, PlatformInfo & platInfo);
