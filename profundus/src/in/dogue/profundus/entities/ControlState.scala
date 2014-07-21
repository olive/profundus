package in.dogue.profundus.entities

import in.dogue.profundus.input.Controls

case class ControlState(isShovelling:Boolean,
                        isClimbing:Boolean,
                        isBombing:Boolean,
                        isRoping:Boolean,
                        isDropping:Boolean) {
  def update(canUseTool:Boolean) = {
    copy(isShovelling=Controls.Space.justPressed && canUseTool,
         isClimbing=Controls.Action.justPressed,
         isBombing=Controls.Capsule.justPressed,
         isRoping=Controls.Rope.justPressed,
         isDropping=Controls.Action.justPressed && Controls.Down.isPressed)
  }
}
