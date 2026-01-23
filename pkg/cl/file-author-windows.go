//go:build windows

package cl

func getFileOwner(path string) string {
	// On Windows, we return empty to signify we can't determine the author
	// which results in 'nil' in the main Call function.
	return ""
}