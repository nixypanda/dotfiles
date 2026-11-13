import { Plugin } from "@opencode/plugin"

export default Plugin.define({
  id: "no-rate-limit-retries",
  async setup(ctx) {
    await ctx.session.hook("retry", (event) => {
      if (event.error.status === 429) {
        event.decision = { retry: false }
      }
    })
  },
})
