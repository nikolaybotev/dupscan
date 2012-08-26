package dupscan

import java.nio.file.FileSystems
import java.nio.file.FileVisitResult
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.SimpleFileVisitor
import java.nio.file.attribute.BasicFileAttributes
import java.security.MessageDigest
import java.util.Formatter

import scala.collection.mutable.HashMap
import scala.collection.mutable.MultiMap
import scala.collection.mutable.Set

object Main extends App {

  val dir = "/Users/ktgrnz/Music"
  val destDir = "/Users/nikolay/MusicDupes"
  val exts = List("mp3", "m4a", "m4p")
  
  val map: MultiMap[String, Path] = new HashMap[String, Set[Path]]() with MultiMap[String, Path]
  
  val md = MessageDigest.getInstance("SHA-512");
  
  def sha(in: Array[Byte]): String = {
    def byteArray2Hex(hash: Array[Byte]): String = {
      val formatter = new Formatter();
      for (b <- hash) {
        formatter.format("%02x", new java.lang.Byte(b))
      }
      return formatter.toString()
    }
    
    return byteArray2Hex(md.digest(in));
  }
  
  val dirPath = Paths.get(dir)
  
  val extPatterns = exts map { e => FileSystems.getDefault().getPathMatcher("glob:*." + e) }
  
  println(extPatterns)
  println(exts)
  
  val visitor = new SimpleFileVisitor[Path]() {
    def matches(f: Path): Boolean = {
      for (p <- extPatterns) {
        if (p. matches(f.getFileName())) {
          return true
        }
      }
      return false
    }
    
    override def visitFile(f: Path, a: BasicFileAttributes): FileVisitResult = {
      if (matches(f)) {
        val key = sha(Files.readAllBytes(f))
        map.addBinding(key, f)
      }
      return super.visitFile(f, a)
    }
    
    override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult = {
      println("Scanning " + dir + " ...")
      return super.preVisitDirectory(dir, attrs)
    }
  }
  
  Files.walkFileTree(dirPath, visitor)

  val dupes = map filter { _._2.size > 1 }
  val files = map flatMap(_._2)

  println("Done. Found " + files.size + " files. " + dupes.size + " duplicates")
  
  val totalSize = files map (Files.size(_)) reduce(_ + _)
  println("Total file size = " + (totalSize / 1024.0d / 1024 / 1024))

  // Check sizes
  for (dupSet <- dupes.values) {
    for (dup1 <- dupSet) {
      for (dup2 <- dupSet) {
        if (Files.size(dup1) != Files.size(dup2)) {
          println("Hash collision between \n  " + dup1 + "\n and \n  " + dup2)
        }
      }
    }
  }
  
  // Move dupes out
  val destDirPath = Paths.get(destDir)
  for (dup <- dupes.values) {
    var first = true
    
    for (m <- dup.toList.sortBy(_.getFileName.toString.length)) {
      if (first) {
        println("Keeping " + m)
        first = false
      } else {
        val destFile = destDirPath.resolve(dirPath.relativize(m))
        println("Moving " + m + " to " + destFile)
        try {
          Files.createDirectories(destFile.getParent())
          Files.move(m, destFile)
        } catch { case e => println("  Error moving: " + e) }
      }
    }
  }

}