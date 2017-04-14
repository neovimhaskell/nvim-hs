# Steps to do during a release

1. Make sure the version in the cabal file matches the planned release
   number

2. Adjust the stack template with that version and test whether it works

3. Run the test suite for good measure

4. Create the haddocks and fix errors/warnings

5. Update the `CHANGELOG.md` file

6. Commit and push everything (do not tag yet)

7. Tag if travis build is okay

8. Generate the haddock documentation (unless you know you don't have to)

9. Generate a distribution tarball

10. Upload tarball and documentation

11. Create a new commit where the version numbers are incremented

