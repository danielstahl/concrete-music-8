package net.soundmining

import net.soundmining.modular.ModularInstrument.ControlInstrument
import net.soundmining.modular.ModularSynth.{lineControl, staticControl}
import net.soundmining.sound.{SoundPlay, SoundPlays}
import net.soundmining.synth.SuperColliderClient.loadDir
import net.soundmining.synth.{Instrument, SuperColliderClient}

object ConcreteMusic8 {

  implicit val client: SuperColliderClient = SuperColliderClient()
  val SOUND_DIR = "/Users/danielstahl/Documents/Music/Pieces/Concrete Music/Concrete Music 8/sounds/"
  val SYNTH_DIR = "/Users/danielstahl/Documents/Projects/soundmining-modular/src/main/sc/synths"

  val CHAIR_DRAG_1 = "chair-drag-1"
  val CHAIR_DRAG_2 = "chair-drag-2"
  val APPLE_BITE = "apple-bite"
  val CHEST_HANDLE = "chest-handle"
  val CHEST_HIT = "chest-hit"
  val CHEST_LID = "chest-lid"
  val CHEST_SCRATCH_1 = "chest-scratch-1"
  val CHEST_SCRATCH_2 = "chest-scratch-2"
  val CLOCK_DOOR = "clock-door"
  val PEN_CLICK = "pen-click"
  val PEN_LID_HIT = "pen-lid-hit"
  val PEN_LID_RATTLE = "pen-lid-rattle"
  val PEN_LID_SCRATCH = "pen-lid-scratch"

  val soundPlays = SoundPlays(
    soundPlays = Map(
      APPLE_BITE -> SoundPlay(s"${SOUND_DIR}/Apple bite.flac", 0.024, 1.156),
      CHAIR_DRAG_1 -> SoundPlay(s"${SOUND_DIR}/Chair drag 1.flac", 0.0, 1.098),
      CHAIR_DRAG_2 -> SoundPlay(s"${SOUND_DIR}/Chair drag 2.flac", 0.196, 1.477),
      CHEST_HANDLE -> SoundPlay(s"${SOUND_DIR}/Chest handle.flac", 0.084, 1.142),
      CHEST_HIT -> SoundPlay(s"${SOUND_DIR}/Chest hit.flac", 0.162, 0.638),
      CHEST_LID -> SoundPlay(s"${SOUND_DIR}/Chest lid.flac", 0.000, 5.145),
      CHEST_SCRATCH_1 -> SoundPlay(s"${SOUND_DIR}/Chest scratch 1.flac", 0.000, 2.159),
      CHEST_SCRATCH_2 -> SoundPlay(s"${SOUND_DIR}/Chest scratch 2.flac", 0.171, 1.695),
      CLOCK_DOOR -> SoundPlay(s"${SOUND_DIR}/Clock door.flac", 0.400, 2.781),
      PEN_CLICK -> SoundPlay(s"${SOUND_DIR}/Pen click.flac", 0.019, 0.317),
      PEN_LID_HIT -> SoundPlay(s"${SOUND_DIR}/Pen lid hit.flac", 0.280, 0.955),
      PEN_LID_RATTLE -> SoundPlay(s"${SOUND_DIR}/Pen lid rattle.flac", 0.122, 2.110),
      PEN_LID_SCRATCH -> SoundPlay(s"${SOUND_DIR}/Pen lid scratch.flac", 0.058, 2.001),
    ),
    numberOfOutputBuses = 2)

  /*
  ("chair-drag-1", 0, 0.6), ("chair-drag-2", 0.5, -0.6)
  ("chest-handle", 4, 0.6), ("chest-lid", 0, -0.6)
  ("chest-scratch-1", 0.6, 0.5), ("chest-hit", 0, -0.5)
  ("pen-lid-hit", 0.2, 0.5), ("chest-hit", 0, -0.5)
  ("pen-lid-rattle", 0, 0.5), ("chest-scratch-2", 0.2, -0.5)
  ("clock-door", 0, 0.5), ("chest-hit", 0.2, -0.5)
  * */

  def playSound(sound: Int): Unit = {
    client.resetClock

    val sounds: Seq[((String, Double, Double), (String, Double, Double))] = Seq(
      (("chair-drag-1", 0, 0.6), ("chair-drag-2", 0.5, -0.6)),
      (("chest-handle", 4, 0.6), ("chest-lid", 0, -0.6)),
      (("chest-scratch-1", 0.6, 0.5), ("chest-hit", 0, -0.5)),
      (("pen-lid-hit", 0.2, 0.5), ("chest-hit", 0, -0.5)),
      (("pen-lid-rattle", 0, 0.5), ("chest-scratch-2", 0.2, -0.5)),
      (("clock-door", 0, 0.5), ("chest-hit", 0.2, -0.5)))

    val (sound1, sound2) = sounds(sound)

    internalPlay(sound1)
    internalPlay(sound2)

    def internalPlay(sound: (String, Double, Double)): Unit = {
      val (audio, start, pan) = sound

      soundPlays.mono(audio)
        .playMono(1.0, 1.0)
        .splay(0.2, pan)
        .play(start, 0)
    }
  }

  def init(): Unit = {
    println("Starting up SuperCollider client")
    client.start
    Instrument.setupNodes(client)
    client.send(loadDir(SYNTH_DIR))
    soundPlays.init
  }

  def stop(): Unit = {
    println("Stopping SuperCollider client")
    client.stop
  }
}
