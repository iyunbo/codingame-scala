package org.iyunbo.coding
package vaccine

import java.io.{FileInputStream, FileOutputStream}
import java.util
import java.util.zip.ZipInputStream
import java.nio.file.{Files, Paths}
import java.nio.file.attribute.PosixFilePermission

object FileHelper {

  def uncompress(zipFile: String): Unit = {
    val fis = new FileInputStream(zipFile)
    val zis = new ZipInputStream(fis)
    LazyList.continually(zis.getNextEntry).takeWhile(_ != null).foreach { file =>
      val fout = new FileOutputStream(file.getName)
      val buffer = new Array[Byte](1024)
      LazyList.continually(zis.read(buffer)).takeWhile(_ != -1).foreach(fout.write(buffer, 0, _))
    }
  }

  def setExecutePermission(file: String): Unit = {



    val perms = new util.HashSet[PosixFilePermission]()
    //add owners permission
    perms.add(PosixFilePermission.OWNER_READ)
    perms.add(PosixFilePermission.OWNER_WRITE)
    perms.add(PosixFilePermission.OWNER_EXECUTE)
    //add group permissions
    perms.add(PosixFilePermission.GROUP_READ)
    perms.add(PosixFilePermission.GROUP_WRITE)
    perms.add(PosixFilePermission.GROUP_EXECUTE)

    Files.setPosixFilePermissions(Paths.get(file), perms)
  }

}
