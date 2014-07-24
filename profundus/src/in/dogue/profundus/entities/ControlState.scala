package in.dogue.profundus.entities

import in.dogue.profundus.input.Controls

case class ControlState(isShovelling:Boolean,
                        isClimbing:Boolean,
                        isBombing:Boolean,
                        isRoping:Boolean,
                        isDropping:Boolean,
                        isFlaring:Boolean,
                        isKicking:Boolean) {
  def update(canUseTool:Boolean) = {
    copy(isShovelling=Controls.Space.justPressed && canUseTool,
         isClimbing=Controls.Action.justPressed,
         isBombing=Controls.Capsule.justPressed,
         isRoping=Controls.Rope.justPressed,
         isDropping=Controls.Drop.justPressed,
         isFlaring=Controls.Action.justPressed && Controls.Up.isPressed,
         isKicking=Controls.Kick.justPressed)
  }
}
