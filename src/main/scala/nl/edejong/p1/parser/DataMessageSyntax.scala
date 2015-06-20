package nl.edejong.p1.parser

import org.joda.time.DateTime
import squants.Time
import squants.electro.ElectricCurrent
import squants.energy.{Energy, Power}
import squants.space.Volume

sealed trait Tariff
case object Tariff1 extends Tariff
case object Tariff2 extends Tariff

sealed trait Direction
case object ToClient extends Direction
case object FromClient extends Direction

sealed trait SwitchPosition
case object In extends SwitchPosition
case object Out extends SwitchPosition
case object Enabled extends SwitchPosition

sealed trait ValvePosition
case object On extends ValvePosition
case object Off extends ValvePosition
case object Released extends ValvePosition

sealed trait Phase
case object L1 extends Phase
case object L2 extends Phase
case object L3 extends Phase

sealed trait AnomalyType
case object Swell extends AnomalyType
case object Sag extends AnomalyType

trait DataMessageSyntax {
  case class OBISReference(
                            major: Int,
                            minor: Int,
                            id1: Int,
                            id2: Int,
                            id3: Int,
                            id4: Int)

  case class P1Telegram(
                        identification: String,
                        objects: Seq[CosemObject])

  sealed trait CosemObject

  case class VersionInformation(version: Int) extends CosemObject
  case class DateTimeStamp(dateTime: DateTime) extends CosemObject
  case class EquipmentIdentifier(id: String) extends CosemObject
  case class MeterReading(energy: Energy, tarriff: Tariff, direction: Direction) extends CosemObject
  case class TariffIndicator(tariff: Tariff) extends CosemObject
  case class CurrentPower(power: Power, direction: Direction) extends CosemObject
  case class ThresholdElectricity(power: Power) extends CosemObject
  case class SwitchPositionElectricity(switchPosition: SwitchPosition) extends CosemObject
  case class NumberOfPowerFailures(failures: Int) extends CosemObject
  case class NumberOfLongPowerFailures(failures: Int) extends CosemObject
  case class PowerFailureEventLog( failure: List[(DateTime, Time)] ) extends CosemObject
  case class VoltageAnomaly(count: Int, phase: Phase, anomalyType: AnomalyType) extends CosemObject
  case class TextMessageCodes(codes: Array[Byte]) extends CosemObject
  case class TextMessage(message: String) extends CosemObject
  case class InstantaneousCurrent(phase: Phase, current: ElectricCurrent) extends CosemObject
  case class InstantaneousPower(phase: Phase, power: Power, direction: Direction) extends CosemObject

  case class MbusEquipmentIdentifier(n: Int, id: String) extends CosemObject
  case class MbusDeviceType(n: Int, deviceType: Int) extends CosemObject
  case class MbusUnitsDeliveredToClient(n: Int, timestamp: DateTime, amount: BigDecimal, units: String) extends CosemObject
  case class MbusValvePosition(n: Int, valvePosition: ValvePosition) extends CosemObject


}
