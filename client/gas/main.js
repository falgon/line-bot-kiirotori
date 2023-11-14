const prop = PropertiesService.getScriptProperties().getProperties()

const makeSignature = (requestBody, secretKey) => {
  return Utilities.base64Encode(
    Utilities.newBlob(
      Utilities.computeHmacSha256Signature(
        Utilities.newBlob(requestBody).getBytes(), 
        Utilities.newBlob(secretKey).getBytes()
      )
    ).getBytes()
  )
}

const postKiirotori = (body, secretKey) => {
  const endpoint = prop.ENDPOINT
  const resp = UrlFetchApp.fetch(endpoint, {
    "method": "post",
    "headers": {
      "x-ext-line-signature": makeSignature(body, secretKey)
    },
    contentType: "application/json",
    "payload": body
  })
  return resp.getResponseCode() == 200
}

const sendLineNotify = (message) => {
  const endpoint = "https://notify-api.line.me/api/notify"
  const token = prop.LINE_NOTIFY_TOKEN
  UrlFetchApp.fetch(endpoint, {
    "method": "post",
    "headers": {
      "Authorization": ["Bearer", token].join(" ")
    },
    "payload": {
      "message": message
    }
  })
}

const sendMessages = (s) => {
  const eventObject = {
    "type": "sendPlain",
    "timestamp": new Date().getMilliseconds(),
    "source": "Google Apps Script - TokyoGASKiirotoriAutoTransfer",
    "webhookEventId": ULID.ulid(), // thx: https://zenn.dev/gyumesy/articles/a3adc6110859ac
    "deliveryContext": {
      "isRedelivery": false
    },
    "messages": s,
    "target": prop.FAMILY_GROUP_ID
  }
  const requestBody = {
    "destination": prop.BOT_USER_ID,
    "events": [eventObject]
  }

  try {
    if (!postKiirotori(JSON.stringify(requestBody), prop.HMAC_SECRET_KEY)) {
      sendLineNotify("TokyoGASKiiroitoriAutoTransfer: unexpected status code")
      throw new Error("unexpected status code")
    }
  } catch(e) {
    sendLineNotify(e)
    throw new Error(e)
  }
}

const checkBillingMain = () => {
  getUnreadBillingMailsWith((month, url) => {
    sendMessages(["東京ガス⚡💨", `${month}月の請求額ガ確定！下記ノURLカラ確認シテネ！\n${url}`])
  })
}

const checkOneTimePWMain = () => {
  getUnreadOneTimePassWith((pw, validLimit) => {
    sendMessages(["東京ガス⚡💨", "ワンタイムパスワードガ発行サレマシタ！", `${pw}\n${validLimit}`])
  })
}
